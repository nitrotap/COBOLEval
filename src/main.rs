//! COBOLEval Transpiler - Converts Python HumanEval tasks to COBOL.
//!
//! This tool reads the HumanEval benchmark dataset and transpiles it to COBOL,
//! creating the COBOLEval benchmark for evaluating LLMs on COBOL code generation.

mod constants;
mod parser;
mod transpiler;
mod types;

use anyhow::Result;
use clap::Parser as ClapParser;
use log::{error, info, warn};
use std::{
    fs::File,
    io::Write,
    path::PathBuf,
};
use tree_sitter::Parser;

use crate::parser::{load_human_eval, parse, parse_arg_list_length, parse_test};
use crate::transpiler::replace_array_length;
use crate::types::CobolEval;

/// COBOLEval Transpiler - Convert HumanEval Python tasks to COBOL
#[derive(ClapParser, Debug)]
#[command(name = "cobol-eval-transpiler")]
#[command(author = "bloop.ai")]
#[command(version)]
#[command(about = "Transpile HumanEval Python benchmark to COBOL", long_about = None)]
struct Args {
    /// Path to the input HumanEval.jsonl file
    #[arg(short, long, default_value = "./data/HumanEval.jsonl")]
    input: PathBuf,

    /// Path to the output CobolEval.jsonl file
    #[arg(short, long, default_value = "./data/CobolEval.jsonl")]
    output: PathBuf,

    /// Verbose output (show all skipped tasks)
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

fn main() -> Result<()> {
    // Initialize logger
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    let args = Args::parse();

    info!("Loading HumanEval from: {:?}", args.input);
    let human_eval = load_human_eval(&args.input);
    info!("Loaded {} tasks from HumanEval", human_eval.len());

    let mut cobol_evals = vec![];
    let mut skipped = 0;
    let mut parse_errors = 0;
    let mut conversion_errors = 0;
    let mut no_tests = 0;

    let mut parser = Parser::new();
    parser.set_language(tree_sitter_python::language())?;

    for eval in human_eval {
        let functions = parse(&mut parser, &eval.prompt);
        match functions {
            Ok(f) => {
                if f.len() > 1 {
                    if args.verbose {
                        warn!("Skipping {} - multiple functions found", eval.entry_point);
                    }
                    skipped += 1;
                    continue;
                }
                let function = f.into_iter().next().unwrap();

                match function.to_cobol() {
                    Ok(mut prompt) => {
                        let tests = parse_test(
                            &mut parser,
                            &eval.test,
                            &eval.entry_point,
                            &function.return_type,
                        );

                        let mut valid_tests = match tests {
                            Ok(ts) => ts
                                .into_iter()
                                .filter_map(|t| {
                                    let converted = t.and_then(|t| t.to_cobol(&function));
                                    if let Err(e) = &converted {
                                        if args.verbose {
                                            warn!(
                                                "Could not convert test for {}: {}",
                                                eval.entry_point, e
                                            );
                                        }
                                        None
                                    } else {
                                        converted.ok()
                                    }
                                })
                                .collect::<Vec<_>>(),
                            Err(e) => {
                                if args.verbose {
                                    error!(
                                        "Could not parse tests for {}: {}",
                                        eval.entry_point, e
                                    );
                                }
                                parse_errors += 1;
                                vec![]
                            }
                        };

                        if valid_tests.is_empty() {
                            if args.verbose {
                                warn!("No valid tests for: {}", eval.entry_point);
                            }
                            no_tests += 1;
                            continue;
                        }

                        // Find number of elements in list arguments and amend types
                        let arg_list_length = parse_arg_list_length(&mut parser, &eval.test);
                        if let Some(len) = arg_list_length {
                            prompt = replace_array_length(&prompt, len);
                            for test in valid_tests.iter_mut() {
                                test.test = replace_array_length(&test.test, len);
                            }
                        }

                        cobol_evals.push(CobolEval {
                            task_id: eval.task_id,
                            prompt,
                            entry_point: eval.entry_point,
                            canonical_solution: eval.canonical_solution,
                            tests: valid_tests,
                        });
                    }
                    Err(e) => {
                        if args.verbose {
                            error!("Could not convert {}: {}", eval.entry_point, e);
                        }
                        conversion_errors += 1;
                        continue;
                    }
                }
            }
            Err(e) => {
                if args.verbose {
                    error!("Could not parse {}: {}", eval.entry_point, e);
                }
                parse_errors += 1;
                continue;
            }
        }
    }

    info!("Writing {} tasks to: {:?}", cobol_evals.len(), args.output);

    let mut cobol_evals_file = File::create(&args.output)?;
    for eval in &cobol_evals {
        let json = serde_json::to_string(&eval)?;
        writeln!(cobol_evals_file, "{}", json)?;
    }

    // Print summary
    info!("=== Transpilation Summary ===");
    info!("Successfully transpiled: {} tasks", cobol_evals.len());
    info!("Skipped (multiple functions): {}", skipped);
    info!("Parse errors: {}", parse_errors);
    info!("Conversion errors: {}", conversion_errors);
    info!("No valid tests: {}", no_tests);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::PyType;

    #[test]
    fn test_pytype_from_str() {
        assert_eq!(PyType::from_str("int").unwrap(), PyType::Int);
        assert_eq!(PyType::from_str("float").unwrap(), PyType::Float);
        assert_eq!(PyType::from_str("str").unwrap(), PyType::String);
        assert_eq!(PyType::from_str("bool").unwrap(), PyType::Bool);
        assert_eq!(
            PyType::from_str("List[int]").unwrap(),
            PyType::List(Box::new(PyType::Int))
        );
    }

    #[test]
    fn test_pytype_to_cobol() {
        assert_eq!(PyType::Int.to_cobol().unwrap(), "PIC S9(10)");
        assert_eq!(PyType::Float.to_cobol().unwrap(), "COMP-2");
        assert_eq!(PyType::String.to_cobol().unwrap(), "PIC X(100)");
        assert_eq!(PyType::Bool.to_cobol().unwrap(), "PIC 9");
    }

    #[test]
    fn test_format_name() {
        use crate::types::format_name;
        assert_eq!(format_name("has_close_elements"), "HAS-CLOSE-ELEMENTS");
        assert_eq!(format_name("myFunction"), "MYFUNCTION");
    }

    #[test]
    fn test_replace_array_length() {
        let src = "OCCURS 100 TIMES INDEXED BY NI";
        let result = replace_array_length(src, 50);
        assert_eq!(result, "OCCURS 50 TIMES INDEXED BY NI");
    }
}
