//! Type definitions for the COBOLEval transpiler.

use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicUsize, Ordering};
use tree_sitter::Node;

use crate::constants::{BOOL_PIC, FLOAT_TYPE, INDEX_CHARS, INT_PIC, STRING_PIC};

/// Global index counter for array indexing.
pub static INDEX: AtomicUsize = AtomicUsize::new(0);

/// Input format from HumanEval benchmark.
#[derive(Deserialize, Debug)]
pub struct HumanEval {
    pub task_id: String,
    pub prompt: String,
    pub entry_point: String,
    pub canonical_solution: String,
    pub test: String,
}

/// Output format for COBOLEval benchmark.
#[derive(Debug, Serialize)]
pub struct CobolEval {
    pub task_id: String,
    pub prompt: String,
    pub entry_point: String,
    pub canonical_solution: String,
    pub tests: Vec<CobolTest>,
}

/// A parsed Python function definition.
#[derive(Debug)]
pub struct PyFunction {
    pub name: String,
    pub args: Vec<PyArgument>,
    pub return_type: PyType,
    pub docstring: String,
}

impl PyFunction {
    /// Convert this function to a COBOL program header/prompt.
    pub fn to_cobol(&self) -> Result<String> {
        let program_name = format_name(&self.name);
        let program_description = self.to_program_description();
        let working_storage_vars = self.to_linkage()?;

        Ok(crate::transpiler::prompt(
            &program_name,
            &program_description,
            &working_storage_vars,
        ))
    }

    /// Generate the LINKAGE SECTION for this function.
    pub fn to_linkage(&self) -> Result<String> {
        INDEX.store(0, Ordering::SeqCst);

        let args = self
            .args
            .iter()
            .map(PyArgument::to_cobol)
            .collect::<Result<Vec<_>>>()?;
        let args = args.join("\n");

        let return_type = self.return_type.to_cobol()?;
        let result_str = format!("           05 RESULT {}.", return_type);
        Ok(format!("       01 LINKED-ITEMS.\n{}\n{}", args, result_str))
    }

    /// Format the docstring as COBOL comments.
    fn to_program_description(&self) -> String {
        self.docstring
            .trim()
            .trim_matches('"')
            .lines()
            .map(str::trim)
            .map(|l| format!("      * {}", l))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// A function argument with name and type.
#[derive(Debug)]
pub struct PyArgument {
    pub name: String,
    pub type_: PyType,
}

impl PyArgument {
    /// Convert this argument to a COBOL linkage variable.
    pub fn to_cobol(&self) -> Result<String> {
        let type_ = self.type_.to_cobol()?;
        Ok(format!(
            "           05 L-{} {}.",
            format_name(&self.name),
            type_
        ))
    }
}

/// Python types that can be mapped to COBOL.
#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum PyType {
    Int,
    Float,
    String,
    Bool,
    Any,
    None,
    List(Box<PyType>),
}

impl PyType {
    /// Parse a Python type annotation string.
    pub fn from_str(s: &str) -> Result<Self> {
        match s {
            "int" => Ok(PyType::Int),
            "float" => Ok(PyType::Float),
            "str" | "Optional[str]" => Ok(PyType::String),
            "bool" => Ok(PyType::Bool),
            "Any" => Ok(PyType::Any),
            "List[int]" | "Tuple[int, int]" => Ok(PyType::List(Box::new(PyType::Int))),
            "List[float]" | "Tuple[float, float]" => Ok(PyType::List(Box::new(PyType::Float))),
            "List[str]" => Ok(PyType::List(Box::new(PyType::String))),
            "List[Any]" => Ok(PyType::List(Box::new(PyType::Any))),
            _ => Err(anyhow!("Could not parse type: {}", s)),
        }
    }

    /// Infer a Python type from an AST node.
    pub fn from_node(node: &Node) -> Result<Self> {
        match node.kind() {
            "integer" => Ok(PyType::Int),
            "float" => Ok(PyType::Float),
            "string" => Ok(PyType::String),
            "none" => Ok(PyType::None),
            "true" | "false" => Ok(PyType::Bool),
            "list" | "tuple" => {
                let inner = node
                    .named_child(0)
                    .map(|n| PyType::from_node(&n))
                    .context("Could not get list inner type")??;
                Ok(PyType::List(Box::new(inner)))
            }
            "unary_operator" => Ok(node
                .named_child(0)
                .map(|n| PyType::from_node(&n))
                .context("Could not get unary inner type")??),
            _ => Err(anyhow!("Could not parse type: {}", node.kind())),
        }
    }

    /// Convert this type to a COBOL type declaration.
    pub fn to_cobol(&self) -> Result<String> {
        match self {
            PyType::Int => Ok(INT_PIC.into()),
            PyType::Float => Ok(FLOAT_TYPE.into()),
            PyType::String => Ok(STRING_PIC.into()),
            PyType::Bool => Ok(BOOL_PIC.into()),
            PyType::List(t) => {
                let inner = t.to_cobol()?;
                let suffix = INDEX_CHARS[INDEX.fetch_add(1, Ordering::SeqCst)];
                Ok(format!("OCCURS 100 TIMES INDEXED BY N{} {}", suffix, inner))
            }
            PyType::Any => Err(anyhow!("Cannot convert Any to COBOL")),
            PyType::None => Err(anyhow!("Cannot convert None to COBOL")),
        }
    }
}

/// A Python value with its runtime value and type.
#[derive(Debug, Clone, Serialize)]
pub struct PyValue {
    pub value: String,
    pub type_: PyType,
}

impl PyValue {
    /// Create a PyValue from an AST node, inferring its type.
    pub fn from_node(s: &Node, src: &str) -> Result<PyValue> {
        let value = s.utf8_text(src.as_bytes()).context("Could not get value")?;
        Ok(PyValue {
            value: value.to_string(),
            type_: PyType::from_node(s)?,
        })
    }

    /// Create a PyValue from an AST node with a known type.
    pub fn from_node_and_type(s: &Node, src: &str, type_: &PyType) -> Result<PyValue> {
        let value = s.utf8_text(src.as_bytes()).context("Could not get value")?;
        Ok(PyValue {
            value: value.to_string(),
            type_: type_.clone(),
        })
    }

    /// Parse a list value into its elements.
    pub fn parse_list(&self) -> Result<Vec<String>> {
        match &self.type_ {
            PyType::List(_) => Ok(self
                .value
                .trim_matches(|c| c == '[' || c == ']' || c == '(' || c == ')')
                .split(',')
                .map(|s| s.trim_start())
                .map(String::from)
                .collect::<Vec<_>>()),
            _ => Err(anyhow!("Cannot parse non-list type")),
        }
    }
}

/// A parsed Python test case.
#[derive(Debug)]
pub struct PyTest {
    pub name: String,
    pub inputs: Vec<PyValue>,
    pub output: PyValue,
}

/// A COBOL test case (generated from PyTest).
#[derive(Debug, Serialize)]
pub struct CobolTest {
    pub test: String,
    pub result: PyValue,
}

/// Format a Python identifier as a COBOL name.
pub fn format_name(name: &str) -> String {
    name.to_uppercase().replace('_', "-")
}
