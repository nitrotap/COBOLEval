use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

/// A single task from CobolEval.jsonl
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CobolEval {
    pub task_id: String,
    pub prompt: String,
    pub entry_point: String,
    pub canonical_solution: String,
    pub tests: Vec<CobolTest>,
}

/// A test case for a COBOL eval task
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CobolTest {
    pub test: String,
    pub result: PyValue,
}

/// Expected result value with type info
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PyValue {
    pub value: String,
    #[serde(rename = "type_")]
    pub type_: PyType,
}

/// Python/COBOL type information
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(untagged)]
pub enum PyType {
    Simple(SimpleType),
    List(HashMap<String, String>),
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum SimpleType {
    Int,
    Float,
    String,
    Bool,
}

impl PyType {
    pub fn is_list(&self) -> bool {
        matches!(self, PyType::List(_))
    }

    pub fn list_inner_type(&self) -> Option<&str> {
        match self {
            PyType::List(map) => map.get("List").map(|s| s.as_str()),
            _ => None,
        }
    }
}

/// Parsed value from COBOL output
#[derive(Debug, Clone)]
pub enum ParsedValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    ListInt(Vec<i64>),
    ListFloat(Vec<f64>),
    ListStr(Vec<String>),
}

/// Result of a single test run
#[derive(Debug, Clone, Serialize)]
pub struct TestResult {
    pub test_num: usize,
    pub passed: bool,
    pub compiled: bool,
    pub expected: String,
    pub actual: Option<String>,
    pub error: Option<String>,
}

/// Result of compiling a COBOL program
#[derive(Debug, Clone)]
pub struct CompileResult {
    pub success: bool,
    pub stderr: String,
}

/// Result of running a compiled COBOL test
#[derive(Debug, Clone)]
pub struct TestRunResult {
    pub compiled: bool,
    pub output: Option<Vec<String>>,
    pub error: Option<String>,
}

/// Mutable state for a task being worked on by the agent
#[derive(Debug)]
pub struct TaskState {
    pub task: CobolEval,
    pub current_solution: Option<String>,
    pub solution_path: PathBuf,
    pub work_dir: PathBuf,
    pub attempts_used: usize,
    pub max_attempts: usize,
    pub test_results: Vec<TestResult>,
    pub last_compile_error: Option<String>,
    pub execute: bool,
    pub timeout_secs: u64,
}

/// Shared state across all tools
pub type SharedState = Arc<Mutex<HashMap<String, TaskState>>>;

/// Output sample record (written to samples_results.jsonl)
#[derive(Debug, Serialize)]
pub struct SampleOutput {
    pub sample_id: usize,
    pub task_id: String,
    pub completion: String,
    pub all_passed: bool,
    pub passed: Vec<bool>,
    pub results: Vec<Option<String>>,
    pub compiled: Vec<bool>,
}

/// LLM provider enum
#[derive(Debug, Clone, clap::ValueEnum)]
pub enum Provider {
    Ollama,
    OpenAI,
    Anthropic,
}
