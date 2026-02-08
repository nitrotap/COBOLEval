//! COBOL code generation module.

use anyhow::{Context, Result};
use regex::Regex;

use crate::types::{format_name, CobolTest, PyFunction, PyTest, PyType};

impl PyTest {
    /// Convert a Python test case to a COBOL test program.
    pub fn to_cobol(&self, function: &PyFunction) -> Result<CobolTest> {
        let program_name = format_name(&self.name);
        let output_record = self.to_output_record(function)?;
        let working_storage = self.to_working_storage(function);
        let linked_items = function.to_linkage()?;
        let data_moves = self.to_data_moves(function);
        let write_logic = self.to_write_logic(function, &linked_items)?;

        Ok(CobolTest {
            test: eval_program(
                &program_name,
                &output_record,
                &working_storage,
                &linked_items,
                &data_moves,
                &write_logic,
            ),
            result: self.output.clone(),
        })
    }

    /// Generate the output record declaration for this test.
    fn to_output_record(&self, function: &PyFunction) -> Result<String> {
        let return_type = match &function.return_type {
            PyType::List(t) => t.as_ref(),
            _ => &function.return_type,
        };

        let return_str = match return_type {
            PyType::Int => format!("{} SIGN LEADING", return_type.to_cobol()?),
            PyType::Float => "PIC X(15)".to_string(), // Float return hack
            _ => return_type.to_cobol()?,
        };

        Ok(format!("       01 OUTPUT-RECORD {}.", return_str))
    }

    /// Generate the working storage section for this test.
    fn to_working_storage(&self, function: &PyFunction) -> String {
        let return_float = match &function.return_type {
            PyType::Float => true,
            PyType::List(inner) if **inner == PyType::Float => true,
            _ => false,
        };

        if return_float {
            r#"       01 PARTS-REPR.
           05 DECIMAL-PART PIC X(9).
           05 FRACTIONAL-PART PIC X(5).
       01 FLOAT-REPR REDEFINES PARTS-REPR PIC S9(9)V9(5) SIGN LEADING."#
                .into()
        } else {
            "".into()
        }
    }

    /// Generate the write logic for outputting results.
    fn to_write_logic(&self, function: &PyFunction, linkage: &str) -> Result<String> {
        match &function.return_type {
            PyType::List(t) => {
                let index = match_result_index(linkage)?;

                match t.as_ref() {
                    PyType::Float => Ok(format!(
                        r#"       PERFORM VARYING N{index} FROM 1 BY 1 UNTIL N{index} > 100
           MOVE RESULT (N{index}) TO FLOAT-REPR
           STRING DECIMAL-PART "." FRACTIONAL-PART INTO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
       END-PERFORM"#
                    )),
                    _ => Ok(format!(
                        r#"       PERFORM VARYING N{index} FROM 1 BY 1 UNTIL N{index} > 100
           MOVE RESULT (N{index}) TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
       END-PERFORM"#
                    )),
                }
            }
            PyType::Float => Ok(r#"       MOVE RESULT TO FLOAT-REPR
       STRING DECIMAL-PART "." FRACTIONAL-PART INTO OUTPUT-RECORD
       WRITE OUTPUT-RECORD"#
                .to_string()),
            _ => {
                Ok("       MOVE RESULT TO OUTPUT-RECORD\n       WRITE OUTPUT-RECORD\n".to_string())
            }
        }
    }

    /// Generate MOVE statements to set up test inputs.
    fn to_data_moves(&self, function: &PyFunction) -> String {
        function
            .args
            .iter()
            .zip(self.inputs.iter())
            .flat_map(|(arg, inp)| {
                let arg_name = format_name(&arg.name);
                match &arg.type_ {
                    PyType::List(_) => inp
                        .parse_list()
                        .unwrap()
                        .iter()
                        .enumerate()
                        .map(|(i, v)| {
                            format!(
                                "       MOVE {v} TO L-{arg_name}({i})",
                                i = i + 1,
                                arg_name = arg_name
                            )
                        })
                        .collect::<Vec<_>>(),
                    _ => vec![format!(
                        "       MOVE {inp} TO L-{arg_name}",
                        inp = inp.value
                    )],
                }
            })
            .collect::<Vec<String>>()
            .join("\n")
    }
}

/// Match the result index character from linkage section.
pub fn match_result_index(linkage: &str) -> Result<String> {
    let regex_pattern = r"           05 RESULT OCCURS.*TIMES INDEXED BY N(\w)";
    let re = Regex::new(regex_pattern)?;
    re.captures(linkage)
        .and_then(|caps: regex::Captures<'_>| caps.get(1).map(|m| m.as_str().to_string()))
        .context("Could not find index")
}

/// Replace array length occurrences in source.
pub fn replace_array_length(src: &str, len: usize) -> String {
    src.lines()
        .map(|line| {
            if line.contains("OCCURS 100 TIMES INDEXED BY") && !line.contains("RESULT") {
                line.replace("100", &len.to_string())
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate a COBOL program prompt/header.
pub fn prompt(program_name: &str, program_description: &str, linkage_vars: &str) -> String {
    format!(
        r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_name}.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       LINKAGE SECTION.

{linkage_vars}

{program_description}

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       WORKING-STORAGE SECTION.

       "#
    )
}

/// Generate a COBOL evaluation/test program.
pub fn eval_program(
    program_name: &str,
    output_record: &str,
    working_storage: &str,
    linked_items: &str,
    data_moves: &str,
    write_logic: &str,
) -> String {
    format!(
        r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_name}-CALL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT OUTPUT-FILE ASSIGN TO "{program_name}.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           STATUS IS OUTPUT-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD OUTPUT-FILE.
{output_record}

       WORKING-STORAGE SECTION.

       01 OUTPUT-FILE-STATUS PIC X(02).

{working_storage}

{linked_items}

       PROCEDURE DIVISION.

{data_moves}

       CALL "{program_name}" USING LINKED-ITEMS

       OPEN OUTPUT OUTPUT-FILE

       IF OUTPUT-FILE-STATUS NOT = "00"
           DISPLAY "ERROR OPENING OUTPUT FILE"
           STOP RUN
        END-IF

{write_logic}

        IF OUTPUT-FILE-STATUS NOT = "00"
            DISPLAY "ERROR WRITING TO OUTPUT FILE"
            STOP RUN
        END-IF

        CLOSE OUTPUT-FILE
        .
       "#
    )
}
