use crate::eval::checker;
use crate::eval::cobol;
use crate::eval::compiler;
use crate::eval::runner;
use crate::eval::types::{LogEntry, LogEntryType, SharedState, TestResult};
use rig::completion::ToolDefinition;
use rig::tool::Tool;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::fs;
use std::time::Duration;

fn now_iso() -> String {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| format!("{}.{:03}", d.as_secs(), d.subsec_millis()))
        .unwrap_or_default()
}

// ============================================================================
// Error type shared by all tools
// ============================================================================

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct EvalToolError(pub String);

// ============================================================================
// ReadTask — Load task details for the agent
// ============================================================================

#[derive(Deserialize)]
pub struct ReadTaskArgs {
    pub task_id: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ReadTask {
    #[serde(skip)]
    pub state: SharedState,
}

impl Tool for ReadTask {
    const NAME: &'static str = "read_task";
    type Error = EvalToolError;
    type Args = ReadTaskArgs;
    type Output = String;

    async fn definition(&self, _prompt: String) -> ToolDefinition {
        ToolDefinition {
            name: "read_task".to_string(),
            description: "Load a COBOL eval task. Returns the COBOL program skeleton (prompt), entry point name, and number of test cases. Call this first to understand what you need to implement.".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "task_id": {
                        "type": "string",
                        "description": "The task identifier, e.g. 'HumanEval/0'"
                    }
                },
                "required": ["task_id"]
            }),
        }
    }

    async fn call(&self, args: Self::Args) -> Result<Self::Output, Self::Error> {
        let input_str = format!("task_id={}", args.task_id);
        let mut state = self.state.lock().map_err(|e| EvalToolError(e.to_string()))?;
        let task_state = state
            .get_mut(&args.task_id)
            .ok_or_else(|| EvalToolError(format!("Task not found: {}", args.task_id)))?;

        let output = format!(
            "Task: {}\nEntry Point: {}\nNumber of tests: {}\n\nCOBOL Prompt (complete the WORKING-STORAGE SECTION and PROCEDURE DIVISION):\n\n{}",
            task_state.task.task_id,
            task_state.task.entry_point,
            task_state.task.tests.len(),
            task_state.task.prompt
        );

        task_state.conversation_log.push(LogEntry {
            timestamp: now_iso(),
            entry_type: LogEntryType::ToolCall,
            tool_name: Some("read_task".to_string()),
            input: input_str,
            output: output.clone(),
        });

        Ok(output)
    }
}

// ============================================================================
// SubmitSolution — Agent submits COBOL code
// ============================================================================

#[derive(Deserialize)]
pub struct SubmitSolutionArgs {
    pub task_id: String,
    pub cobol_code: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct SubmitSolution {
    #[serde(skip)]
    pub state: SharedState,
}

impl Tool for SubmitSolution {
    const NAME: &'static str = "submit_solution";
    type Error = EvalToolError;
    type Args = SubmitSolutionArgs;
    type Output = String;

    async fn definition(&self, _prompt: String) -> ToolDefinition {
        ToolDefinition {
            name: "submit_solution".to_string(),
            description: "Submit your COBOL solution code for a task. Provide the WORKING-STORAGE SECTION variables and PROCEDURE DIVISION logic. The code will be combined with the task prompt to form the complete program. You can submit code directly (no need for markdown code fences).".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "task_id": {
                        "type": "string",
                        "description": "The task identifier"
                    },
                    "cobol_code": {
                        "type": "string",
                        "description": "Your COBOL code (WORKING-STORAGE variables and PROCEDURE DIVISION logic). Must store result in RESULT variable and end with GOBACK."
                    }
                },
                "required": ["task_id", "cobol_code"]
            }),
        }
    }

    async fn call(&self, args: Self::Args) -> Result<Self::Output, Self::Error> {
        let input_str = format!("task_id={}, cobol_code=<{} chars>", args.task_id, args.cobol_code.len());
        let mut state = self.state.lock().map_err(|e| EvalToolError(e.to_string()))?;
        let task_state = state
            .get_mut(&args.task_id)
            .ok_or_else(|| EvalToolError(format!("Task not found: {}", args.task_id)))?;

        if task_state.attempts_used >= task_state.max_attempts {
            return Err(EvalToolError("Maximum attempts reached".to_string()));
        }

        task_state.attempts_used += 1;

        // Try to extract code block if wrapped in markdown fences
        let code = cobol::extract_code_block(&args.cobol_code)
            .unwrap_or_else(|| args.cobol_code.clone());

        // Construct the full program
        let full_program = cobol::construct_program(&task_state.task.prompt, &code);

        // Write to file
        let solution_path = &task_state.solution_path;
        fs::write(solution_path, &full_program)
            .map_err(|e| EvalToolError(format!("Failed to write solution: {}", e)))?;

        task_state.current_solution = Some(full_program.clone());
        task_state.test_results.clear();

        let output = format!(
            "Solution submitted (attempt {}/{}). File written to {:?}. Use compile_solution to check if it compiles.",
            task_state.attempts_used, task_state.max_attempts, solution_path
        );

        task_state.conversation_log.push(LogEntry {
            timestamp: now_iso(),
            entry_type: LogEntryType::ToolCall,
            tool_name: Some("submit_solution".to_string()),
            input: format!("{}\n---\n{}", input_str, args.cobol_code),
            output: output.clone(),
        });

        Ok(output)
    }
}

// ============================================================================
// CompileSolution — Compile submitted code with GnuCOBOL
// ============================================================================

#[derive(Deserialize)]
pub struct CompileSolutionArgs {
    pub task_id: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct CompileSolution {
    #[serde(skip)]
    pub state: SharedState,
}

impl Tool for CompileSolution {
    const NAME: &'static str = "compile_solution";
    type Error = EvalToolError;
    type Args = CompileSolutionArgs;
    type Output = String;

    async fn definition(&self, _prompt: String) -> ToolDefinition {
        ToolDefinition {
            name: "compile_solution".to_string(),
            description: "Compile the submitted COBOL solution using GnuCOBOL (cobc). Returns 'Compilation successful' or the compiler error messages. If compilation fails, fix the errors and re-submit.".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "task_id": {
                        "type": "string",
                        "description": "The task identifier"
                    }
                },
                "required": ["task_id"]
            }),
        }
    }

    async fn call(&self, args: Self::Args) -> Result<Self::Output, Self::Error> {
        let input_str = format!("task_id={}", args.task_id);
        let mut state = self.state.lock().map_err(|e| EvalToolError(e.to_string()))?;
        let task_state = state
            .get_mut(&args.task_id)
            .ok_or_else(|| EvalToolError(format!("Task not found: {}", args.task_id)))?;

        if task_state.current_solution.is_none() {
            return Err(EvalToolError(
                "No solution submitted yet. Use submit_solution first.".to_string(),
            ));
        }

        let timeout = Duration::from_secs(task_state.timeout_secs);
        let result = compiler::compile_solution_only(&task_state.solution_path, timeout);

        task_state.last_compile_error = if result.success {
            None
        } else {
            Some(result.stderr.clone())
        };

        let output = if result.success {
            "Compilation successful! Use run_tests to execute test cases.".to_string()
        } else {
            format!(
                "Compilation FAILED. Compiler errors:\n\n{}",
                result.stderr
            )
        };

        task_state.conversation_log.push(LogEntry {
            timestamp: now_iso(),
            entry_type: LogEntryType::ToolCall,
            tool_name: Some("compile_solution".to_string()),
            input: input_str,
            output: output.clone(),
        });

        Ok(output)
    }
}

// ============================================================================
// RunTests — Execute against all test cases
// ============================================================================

#[derive(Deserialize)]
pub struct RunTestsArgs {
    pub task_id: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct RunTests {
    #[serde(skip)]
    pub state: SharedState,
}

impl Tool for RunTests {
    const NAME: &'static str = "run_tests";
    type Error = EvalToolError;
    type Args = RunTestsArgs;
    type Output = String;

    async fn definition(&self, _prompt: String) -> ToolDefinition {
        ToolDefinition {
            name: "run_tests".to_string(),
            description: "Run the compiled COBOL solution against all test cases. Returns per-test results showing pass/fail, expected output, and actual output. If tests fail, analyze the differences and revise your solution.".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "task_id": {
                        "type": "string",
                        "description": "The task identifier"
                    }
                },
                "required": ["task_id"]
            }),
        }
    }

    async fn call(&self, args: Self::Args) -> Result<Self::Output, Self::Error> {
        let input_str = format!("task_id={}", args.task_id);
        let mut state = self.state.lock().map_err(|e| EvalToolError(e.to_string()))?;
        let task_state = state
            .get_mut(&args.task_id)
            .ok_or_else(|| EvalToolError(format!("Task not found: {}", args.task_id)))?;

        if task_state.current_solution.is_none() {
            return Err(EvalToolError(
                "No solution submitted. Use submit_solution first.".to_string(),
            ));
        }

        let timeout = Duration::from_secs(task_state.timeout_secs);
        let tests = task_state.task.tests.clone();
        let solution_path = task_state.solution_path.clone();
        let work_dir = task_state.work_dir.clone();
        let entry_point = task_state.task.entry_point.clone();
        let execute = task_state.execute;

        let mut test_results = Vec::new();

        for (i, test) in tests.iter().enumerate() {
            let run_result = runner::run_test(
                &entry_point,
                &solution_path,
                test,
                &work_dir,
                timeout,
                execute,
            );

            let mut passed = false;
            let mut actual_str = None;

            if let Some(ref output) = run_result.output {
                // Parse expected value
                let expected_len = checker::expected_list_len(&test.result.value);
                if let Ok(parsed_result) = checker::parse_result(output, &test.result.type_, expected_len) {
                    if let Ok(parsed_expected) = checker::parse_expected(&test.result.value, &test.result.type_) {
                        passed = checker::is_equal(&test.result.type_, &parsed_result, &parsed_expected);
                    }
                    actual_str = Some(format!("{:?}", parsed_result));
                }
            }

            test_results.push(TestResult {
                test_num: i + 1,
                passed,
                compiled: run_result.compiled,
                expected: test.result.value.clone(),
                actual: actual_str.or(run_result.error.clone()),
                error: run_result.error,
            });
        }

        task_state.test_results = test_results.clone();

        let all_passed = test_results.iter().all(|t| t.passed);
        let passed_count = test_results.iter().filter(|t| t.passed).count();
        let total = test_results.len();
        let compiled_count = test_results.iter().filter(|t| t.compiled).count();

        let detail: Vec<String> = test_results
            .iter()
            .map(|t| {
                format!(
                    "  Test {}: {} (compiled: {}, expected: {}, actual: {})",
                    t.test_num,
                    if t.passed { "PASS" } else { "FAIL" },
                    t.compiled,
                    t.expected,
                    t.actual.as_deref().unwrap_or("N/A")
                )
            })
            .collect();

        let output = format!(
            "Test Results: {}/{} passed, {}/{} compiled{}\n\n{}",
            passed_count,
            total,
            compiled_count,
            total,
            if all_passed {
                " - ALL TESTS PASSED!"
            } else {
                ""
            },
            detail.join("\n")
        );

        task_state.conversation_log.push(LogEntry {
            timestamp: now_iso(),
            entry_type: LogEntryType::ToolCall,
            tool_name: Some("run_tests".to_string()),
            input: input_str,
            output: output.clone(),
        });

        Ok(output)
    }
}

// ============================================================================
// GetTaskStatus — Check current progress
// ============================================================================

#[derive(Deserialize)]
pub struct GetTaskStatusArgs {
    pub task_id: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct GetTaskStatus {
    #[serde(skip)]
    pub state: SharedState,
}

impl Tool for GetTaskStatus {
    const NAME: &'static str = "get_task_status";
    type Error = EvalToolError;
    type Args = GetTaskStatusArgs;
    type Output = String;

    async fn definition(&self, _prompt: String) -> ToolDefinition {
        ToolDefinition {
            name: "get_task_status".to_string(),
            description: "Check the current status of a task: attempts used, last compile error, test results summary.".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "task_id": {
                        "type": "string",
                        "description": "The task identifier"
                    }
                },
                "required": ["task_id"]
            }),
        }
    }

    async fn call(&self, args: Self::Args) -> Result<Self::Output, Self::Error> {
        let input_str = format!("task_id={}", args.task_id);
        let mut state = self.state.lock().map_err(|e| EvalToolError(e.to_string()))?;
        let task_state = state
            .get_mut(&args.task_id)
            .ok_or_else(|| EvalToolError(format!("Task not found: {}", args.task_id)))?;

        let passed_count = task_state.test_results.iter().filter(|t| t.passed).count();
        let total = task_state.test_results.len();

        let output = format!(
            "Task: {}\nAttempts: {}/{}\nHas solution: {}\nLast compile error: {}\nTest results: {}/{} passed",
            task_state.task.task_id,
            task_state.attempts_used,
            task_state.max_attempts,
            task_state.current_solution.is_some(),
            task_state.last_compile_error.as_deref().unwrap_or("None"),
            passed_count,
            total
        );

        task_state.conversation_log.push(LogEntry {
            timestamp: now_iso(),
            entry_type: LogEntryType::ToolCall,
            tool_name: Some("get_task_status".to_string()),
            input: input_str,
            output: output.clone(),
        });

        Ok(output)
    }
}
