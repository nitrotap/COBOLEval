use clap::Parser;
use cobol_eval::eval::tools::{CompileSolution, GetTaskStatus, ReadTask, RunTests, SubmitSolution};
use cobol_eval::eval::types::{CobolEval, LogEntry, LogEntryType, Provider, SampleOutput, SharedState, TaskState};
use cobol_eval::eval::metrics;
use rig::completion::Prompt;
use rig::prelude::*;
use rig::providers;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

/// COBOLEval Agentic Orchestrator — an AI agent that solves COBOL programming tasks
#[derive(Parser, Debug)]
#[command(name = "eval_agent", version, about)]
struct Args {
    /// LLM provider: ollama, openai, or anthropic
    #[arg(long, default_value = "ollama", value_enum)]
    provider: Provider,

    /// Model name (e.g., llama3.2, gpt-4o, claude-3-5-sonnet)
    #[arg(long, default_value = "llama3.2")]
    model: String,

    /// Number of independent solution attempts per task
    #[arg(long, default_value = "1")]
    samples_per_task: usize,

    /// Max compile/fix iterations the agent gets per attempt
    #[arg(long, default_value = "3")]
    max_attempts: usize,

    /// LLM temperature
    #[arg(long, default_value = "0.0")]
    temperature: f64,

    /// Output directory for results
    #[arg(long, default_value = "./preds/eval_agent")]
    output_dir: PathBuf,

    /// Timeout in seconds for compilation/execution
    #[arg(long, default_value = "10")]
    timeout: u64,

    /// Path to CobolEval.jsonl dataset
    #[arg(long, default_value = "./data/CobolEval.jsonl")]
    data_path: PathBuf,

    /// Actually execute compiled COBOL (WARNING: runs untrusted code)
    #[arg(long, default_value = "false")]
    execute: bool,
}

const SYSTEM_PROMPT: &str = r#"You are a COBOL programming agent. Your job is to solve programming tasks by writing COBOL code.

For each task:
1. Use read_task to see the COBOL program skeleton and requirements
2. Write a complete COBOL solution (WORKING-STORAGE SECTION variables + PROCEDURE DIVISION logic)
3. Use submit_solution to submit your code
4. Use compile_solution to check if it compiles with GnuCOBOL
5. If compilation fails, read the errors carefully and fix your code — then resubmit
6. Once it compiles, use run_tests to execute against test cases
7. If tests fail, analyze the expected vs actual output and revise your solution

Important COBOL rules:
- Store your result in the RESULT variable defined in the LINKAGE SECTION
- End your PROCEDURE DIVISION with GOBACK
- Use proper COBOL formatting (columns matter in fixed format)
- Variables in WORKING-STORAGE start at column 8
- Statements in PROCEDURE DIVISION start at column 8
- The LINKAGE SECTION is already defined — do NOT redefine it
"#;

fn load_tasks(path: &PathBuf) -> anyhow::Result<Vec<CobolEval>> {
    let content = fs::read_to_string(path)?;
    let tasks: Vec<CobolEval> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(tasks)
}

fn create_state(
    task: &CobolEval,
    output_dir: &PathBuf,
    max_attempts: usize,
    execute: bool,
    timeout: u64,
) -> TaskState {
    let solutions_dir = output_dir.join("solutions");
    let _ = fs::create_dir_all(&solutions_dir);

    TaskState {
        task: task.clone(),
        current_solution: None,
        solution_path: solutions_dir.join(format!("{}.cbl", task.entry_point)),
        work_dir: output_dir.clone(),
        attempts_used: 0,
        max_attempts,
        test_results: Vec::new(),
        last_compile_error: None,
        execute,
        timeout_secs: timeout,
        conversation_log: Vec::new(),
    }
}

fn now_iso() -> String {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| format!("{}.{:03}", d.as_secs(), d.subsec_millis()))
        .unwrap_or_default()
}

/// Sanitize a task_id like "HumanEval/0" into a filename-safe string like "HumanEval_0"
fn sanitize_task_id(task_id: &str) -> String {
    task_id.replace('/', "_")
}

/// Write conversation log for a task to a JSONL file
fn write_conversation_log(logs_dir: &PathBuf, task_id: &str, sample_k: usize, log: &[LogEntry]) -> anyhow::Result<()> {
    let _ = fs::create_dir_all(logs_dir);
    let filename = format!("{}__sample_{}.jsonl", sanitize_task_id(task_id), sample_k);
    let path = logs_dir.join(filename);
    let mut output = String::new();
    for entry in log {
        output.push_str(&serde_json::to_string(entry)?);
        output.push('\n');
    }
    fs::write(&path, &output)?;
    tracing::info!("Conversation log written to {:?} ({} entries)", path, log.len());
    Ok(())
}

/// Run the eval loop with a specific provider client.
/// Generic over the client type to handle Ollama/OpenAI/Anthropic uniformly.
async fn run_eval<C>(
    client: C,
    model: &str,
    args: &Args,
    tasks: &[CobolEval],
) -> anyhow::Result<Vec<SampleOutput>>
where
    C: CompletionClient + Clone,
    C::CompletionModel: rig::completion::CompletionModel + 'static,
{
    let callers_dir = args.output_dir.join("callers");
    let _ = fs::create_dir_all(&callers_dir);

    let mut all_samples = Vec::new();

    for (task_idx, task) in tasks.iter().enumerate() {
        for sample_k in 0..args.samples_per_task {
            tracing::info!(
                "[{}/{}] Task {} (sample {})",
                task_idx + 1,
                tasks.len(),
                task.task_id,
                sample_k
            );

            // Create fresh shared state for this task/sample
            let state: SharedState = Arc::new(Mutex::new(HashMap::new()));
            {
                let mut map = state.lock().unwrap();
                map.insert(
                    task.task_id.clone(),
                    create_state(task, &args.output_dir, args.max_attempts, args.execute, args.timeout),
                );
            }

            // Build agent with tools
            let agent = client
                .agent(model)
                .preamble(SYSTEM_PROMPT)
                .temperature(args.temperature)
                .max_tokens(4096)
                .tool(ReadTask {
                    state: state.clone(),
                })
                .tool(SubmitSolution {
                    state: state.clone(),
                })
                .tool(CompileSolution {
                    state: state.clone(),
                })
                .tool(RunTests {
                    state: state.clone(),
                })
                .tool(GetTaskStatus {
                    state: state.clone(),
                })
                .build();

            // Run the agent
            let prompt = format!(
                "Solve task {}. Use read_task to begin, then write and submit your COBOL solution.",
                task.task_id
            );

            // Log the agent prompt
            {
                let mut map = state.lock().unwrap();
                if let Some(ts) = map.get_mut(&task.task_id) {
                    ts.conversation_log.push(LogEntry {
                        timestamp: now_iso(),
                        entry_type: LogEntryType::AgentPrompt,
                        tool_name: None,
                        input: prompt.clone(),
                        output: String::new(),
                    });
                }
            }

            match agent.prompt(&prompt).await {
                Ok(response) => {
                    tracing::info!("Agent response: {}", &response[..response.len().min(200)]);
                    // Log the agent response
                    let mut map = state.lock().unwrap();
                    if let Some(ts) = map.get_mut(&task.task_id) {
                        ts.conversation_log.push(LogEntry {
                            timestamp: now_iso(),
                            entry_type: LogEntryType::AgentResponse,
                            tool_name: None,
                            input: String::new(),
                            output: response,
                        });
                    }
                }
                Err(e) => {
                    tracing::error!("Agent error for {}: {}", task.task_id, e);
                    // Log the error as agent response
                    let mut map = state.lock().unwrap();
                    if let Some(ts) = map.get_mut(&task.task_id) {
                        ts.conversation_log.push(LogEntry {
                            timestamp: now_iso(),
                            entry_type: LogEntryType::AgentResponse,
                            tool_name: None,
                            input: String::new(),
                            output: format!("ERROR: {}", e),
                        });
                    }
                }
            }

            // Write conversation log for this task/sample
            {
                let map = state.lock().unwrap();
                if let Some(ts) = map.get(&task.task_id) {
                    let logs_dir = args.output_dir.join("logs");
                    if let Err(e) = write_conversation_log(&logs_dir, &task.task_id, sample_k, &ts.conversation_log) {
                        tracing::error!("Failed to write conversation log for {}: {}", task.task_id, e);
                    }
                }
            }

            // Extract results from state
            let map = state.lock().unwrap();
            let task_state = map.get(&task.task_id);

            let sample = if let Some(ts) = task_state {
                SampleOutput {
                    sample_id: sample_k,
                    task_id: task.task_id.clone(),
                    completion: ts.current_solution.clone().unwrap_or_default(),
                    all_passed: ts.test_results.iter().all(|t| t.passed)
                        && !ts.test_results.is_empty(),
                    passed: ts.test_results.iter().map(|t| t.passed).collect(),
                    results: ts
                        .test_results
                        .iter()
                        .map(|t| t.actual.clone())
                        .collect(),
                    compiled: ts.test_results.iter().map(|t| t.compiled).collect(),
                }
            } else {
                SampleOutput {
                    sample_id: sample_k,
                    task_id: task.task_id.clone(),
                    completion: String::new(),
                    all_passed: false,
                    passed: vec![],
                    results: vec![],
                    compiled: vec![],
                }
            };

            all_samples.push(sample);
        }
    }

    Ok(all_samples)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_target(false)
        .init();

    let args = Args::parse();

    tracing::info!("Loading tasks from {:?}", args.data_path);
    let tasks = load_tasks(&args.data_path)?;
    tracing::info!("Loaded {} tasks", tasks.len());

    let _ = fs::create_dir_all(&args.output_dir);

    // Dispatch based on provider
    let samples = match args.provider {
        Provider::Ollama => {
            tracing::info!("Using Ollama provider with model: {}", args.model);
            let client = providers::ollama::Client::from_env();
            run_eval(client, &args.model, &args, &tasks).await?
        }
        Provider::OpenAI => {
            tracing::info!("Using OpenAI provider with model: {}", args.model);
            let client = providers::openai::Client::from_env();
            run_eval(client, &args.model, &args, &tasks).await?
        }
        Provider::Anthropic => {
            tracing::info!("Using Anthropic provider with model: {}", args.model);
            let client = providers::anthropic::Client::from_env();
            run_eval(client, &args.model, &args, &tasks).await?
        }
    };

    // Write results
    let results_path = args.output_dir.join("samples_results.jsonl");
    let mut output = String::new();
    for sample in &samples {
        output.push_str(&serde_json::to_string(sample)?);
        output.push('\n');
    }
    fs::write(&results_path, &output)?;
    tracing::info!("Results written to {:?}", results_path);

    // Compute and print Pass@k
    let mut task_results: HashMap<String, Vec<bool>> = HashMap::new();
    for sample in &samples {
        task_results
            .entry(sample.task_id.clone())
            .or_default()
            .push(sample.all_passed);
    }

    let per_task: Vec<(usize, usize)> = task_results
        .values()
        .map(|results| {
            let n = results.len();
            let c = results.iter().filter(|&&r| r).count();
            (n, c)
        })
        .collect();

    let ks = vec![1];
    let pass_at_k = metrics::compute_pass_at_k(&per_task, &ks);

    println!("\n=== COBOLEval Results ===");
    println!("Tasks: {}", tasks.len());
    println!("Samples per task: {}", args.samples_per_task);
    println!("Max attempts per sample: {}", args.max_attempts);
    println!("Provider: {:?} ({})", args.provider, args.model);
    for (k, v) in &pass_at_k {
        println!("{}: {:.4}", k, v);
    }

    let total_passed = samples.iter().filter(|s| s.all_passed).count();
    println!(
        "Total samples passed: {}/{}",
        total_passed,
        samples.len()
    );

    Ok(())
}
