use regex::Regex;

/// Extract the first fenced code block from a markdown response.
/// Handles ```cobol, ```COBOL, or plain ``` blocks.
pub fn extract_code_block(response: &str) -> Option<String> {
    let re = Regex::new(r"(?si)```(?:cobol)?\s*\n(.*?)```").unwrap();
    re.captures(response)
        .map(|caps| caps.get(1).unwrap().as_str().to_string())
}

/// Swap the Working Storage and Linkage Sections to produce valid COBOL.
/// Port of Python swap_sections() from generate.py.
///
/// The LLM generates code top-to-bottom (WORKING-STORAGE then LINKAGE then PROCEDURE),
/// but the skeleton has LINKAGE before WORKING-STORAGE. This function rearranges them.
pub fn swap_sections(src: &str) -> String {
    let mut begin = Vec::new();
    let mut working_storage = Vec::new();
    let mut linkage = Vec::new();
    let mut procedure = Vec::new();

    let mut current: &mut Vec<String> = &mut begin;

    for line in src.lines() {
        let stripped = line.trim().to_uppercase();
        if stripped.starts_with("WORKING-STORAGE SECTION.") {
            current = &mut working_storage;
        } else if stripped.starts_with("LINKAGE SECTION.") {
            current = &mut linkage;
        } else if stripped.starts_with("PROCEDURE DIVISION") {
            current = &mut procedure;
            // Rewrite PROCEDURE DIVISION header to include USING LINKED-ITEMS
            current.push("       PROCEDURE DIVISION USING LINKED-ITEMS.".to_string());
            continue;
        }
        current.push(line.to_string());
    }

    let mut result = begin;
    result.extend(working_storage);
    result.extend(linkage);
    result.extend(procedure);
    result.join("\n")
}

/// Construct a full COBOL program from a prompt skeleton and LLM completion.
/// Port of Python construct() from generate.py.
pub fn construct_program(prompt: &str, completion: &str) -> String {
    let mut sol = completion.to_string();

    // If the completion starts with WORKING-STORAGE SECTION., strip it
    // (the prompt already has it)
    if sol.trim().starts_with("WORKING-STORAGE SECTION.") {
        sol = sol.replacen("WORKING-STORAGE SECTION.", "", 1);
    }

    let prog = format!("{}\n{}", prompt, sol);
    swap_sections(&prog)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_code_block_cobol() {
        let response = "Here is the solution:\n```cobol\n       WORKING-STORAGE SECTION.\n       01 WS-I PIC 9.\n       PROCEDURE DIVISION.\n       GOBACK.\n```\nDone.";
        let block = extract_code_block(response).unwrap();
        assert!(block.contains("WORKING-STORAGE SECTION"));
        assert!(block.contains("GOBACK"));
    }

    #[test]
    fn test_extract_code_block_plain() {
        let response = "```\n       MOVE 1 TO RESULT.\n```";
        let block = extract_code_block(response).unwrap();
        assert!(block.contains("MOVE 1 TO RESULT"));
    }

    #[test]
    fn test_extract_code_block_none() {
        let response = "No code block here.";
        assert!(extract_code_block(response).is_none());
    }

    #[test]
    fn test_swap_sections() {
        let src = "\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LINKED-ITEMS.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC 9.
       PROCEDURE DIVISION.
       MOVE 1 TO RESULT.";

        let result = swap_sections(src);
        // Working storage should come before linkage
        let ws_pos = result.find("WORKING-STORAGE").unwrap();
        let link_pos = result.find("LINKAGE").unwrap();
        let proc_pos = result.find("PROCEDURE DIVISION").unwrap();
        assert!(ws_pos < link_pos);
        assert!(link_pos < proc_pos);
        assert!(result.contains("USING LINKED-ITEMS"));
    }
}
