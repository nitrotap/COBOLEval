use crate::eval::types::CompileResult;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

/// Compile a COBOL solution file only (no test harness).
/// Uses: cobc -w -fformat=variable {solution_path}
pub fn compile_solution_only(solution_path: &Path, _timeout: Duration) -> CompileResult {
    let output = Command::new("cobc")
        .args(["-w", "-fformat=variable"])
        .arg(solution_path)
        .output();

    match output {
        Ok(out) => CompileResult {
            success: out.status.success(),
            stderr: String::from_utf8_lossy(&out.stderr).to_string(),
        },
        Err(e) => CompileResult {
            success: false,
            stderr: format!("Failed to run cobc: {}", e),
        },
    }
}

/// Compile a COBOL solution with its test harness into an executable.
/// Uses: cobc -w -fformat=variable -x {call_path} {solution_path}
pub fn compile_with_test(
    solution_path: &Path,
    call_path: &Path,
    _timeout: Duration,
) -> CompileResult {
    let output = Command::new("cobc")
        .args(["-w", "-fformat=variable", "-x"])
        .arg(call_path)
        .arg(solution_path)
        .output();

    match output {
        Ok(out) => CompileResult {
            success: out.status.success(),
            stderr: String::from_utf8_lossy(&out.stderr).to_string(),
        },
        Err(e) => CompileResult {
            success: false,
            stderr: format!("Failed to run cobc: {}", e),
        },
    }
}
