use crate::eval::compiler;
use crate::eval::types::{CobolTest, TestRunResult};
use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

/// Format an entry point name into COBOL program name format.
/// e.g., "has_close_elements" -> "HAS-CLOSE-ELEMENTS"
pub fn format_cobol_name(entry_point: &str) -> String {
    entry_point.to_uppercase().replace('_', "-")
}

/// Run a single test case against a compiled COBOL solution.
///
/// 1. Writes the test harness to a .cbl file
/// 2. Compiles solution + test together
/// 3. Optionally executes the resulting binary
/// 4. Reads output from the generated .TXT file
/// 5. Cleans up artifacts
pub fn run_test(
    entry_point: &str,
    solution_path: &Path,
    test: &CobolTest,
    work_dir: &Path,
    timeout: Duration,
    execute: bool,
) -> TestRunResult {
    let cobol_name = format_cobol_name(entry_point);
    let call_name = format!("call_{}", entry_point);
    let call_path = work_dir.join(format!("{}.cbl", call_name));
    let output_file = format!("{}.TXT", cobol_name);

    // Write test harness
    if let Err(e) = fs::write(&call_path, &test.test) {
        return TestRunResult {
            compiled: false,
            output: None,
            error: Some(format!("Failed to write test harness: {}", e)),
        };
    }

    // Compile solution + test
    let compile_result = compiler::compile_with_test(solution_path, &call_path, timeout);
    if !compile_result.success {
        cleanup(&call_name, &output_file);
        return TestRunResult {
            compiled: false,
            output: None,
            error: Some(compile_result.stderr),
        };
    }

    if !execute {
        cleanup(&call_name, &output_file);
        return TestRunResult {
            compiled: true,
            output: None,
            error: Some("Execution disabled (use --execute flag)".to_string()),
        };
    }

    // Execute the compiled test
    let exec_name = format!("./{}", call_name);
    let exec_result = Command::new(&exec_name).output();

    match exec_result {
        Ok(out) => {
            if !out.status.success() {
                let stderr = String::from_utf8_lossy(&out.stderr).to_string();
                cleanup(&call_name, &output_file);
                return TestRunResult {
                    compiled: true,
                    output: None,
                    error: Some(format!("Runtime error: {}", stderr)),
                };
            }

            // Read output file
            let output = match fs::read_to_string(&output_file) {
                Ok(content) => {
                    let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();
                    Some(lines)
                }
                Err(e) => {
                    cleanup(&call_name, &output_file);
                    return TestRunResult {
                        compiled: true,
                        output: None,
                        error: Some(format!("Failed to read output file: {}", e)),
                    };
                }
            };

            cleanup(&call_name, &output_file);
            TestRunResult {
                compiled: true,
                output,
                error: None,
            }
        }
        Err(e) => {
            cleanup(&call_name, &output_file);
            TestRunResult {
                compiled: true,
                output: None,
                error: Some(format!("Failed to execute: {}", e)),
            }
        }
    }
}

/// Clean up generated artifacts
fn cleanup(call_name: &str, output_file: &str) {
    let _ = fs::remove_file(call_name);
    let _ = fs::remove_file(format!("{}.cbl", call_name));
    let _ = fs::remove_file(output_file);
}
