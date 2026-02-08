//! Parser module for extracting Python functions and tests from source code.

use anyhow::{anyhow, Context, Result};
use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};
use tree_sitter::{Node, Parser};

use crate::types::{HumanEval, PyArgument, PyFunction, PyTest, PyType, PyValue};

/// Load HumanEval problems from a JSONL file.
pub fn load_human_eval(path: &Path) -> Vec<HumanEval> {
    let file = File::open(path).expect("Could not open file");
    let reader = BufReader::new(file);

    reader
        .lines()
        .map(|l| l.expect("Could not parse line"))
        .map(|l| serde_json::from_str::<HumanEval>(&l).expect("Could not parse JSON"))
        .collect()
}

/// Get a child node's text by field name.
pub fn get_child_by_field_name<'a>(
    field_name: &str,
    node: &'a Node<'a>,
    src: &'a str,
) -> Result<&'a str> {
    node.child_by_field_name(field_name)
        .and_then(|n| n.utf8_text(src.as_bytes()).ok())
        .ok_or_else(|| anyhow!("Field {} not found", field_name))
}

/// Parse Python source code to extract function definitions.
pub fn parse(parser: &mut Parser, src: &str) -> Result<Vec<PyFunction>> {
    let tree = parser.parse(src, None).context("Could not parse")?;
    let root_node = tree.root_node();

    let query = tree_sitter::Query::new(
        tree_sitter_python::language(),
        "(function_definition) @func",
    )?;
    let mut query_cursor = tree_sitter::QueryCursor::new();
    let matches = query_cursor.captures(&query, root_node, src.as_bytes());

    matches
        .flat_map(|(match_, _)| {
            match_.captures.iter().map(move |capture| {
                let func_node = capture.node;
                let func_name = get_child_by_field_name("name", &func_node, src)?;
                let return_type = get_child_by_field_name("return_type", &func_node, src)?;
                let docstring = get_child_by_field_name("body", &func_node, src)?;

                let typed_params = func_node
                    .child_by_field_name("parameters")
                    .context("Parameters not found")?
                    .named_children(&mut func_node.walk())
                    .filter(|n| n.kind() == "typed_parameter")
                    .map(|n| {
                        let name = n
                            .child(0)
                            .and_then(|n| n.utf8_text(src.as_bytes()).ok())
                            .context("Could not get name")?;
                        let type_ = get_child_by_field_name("type", &n, src)?;
                        Ok(PyArgument {
                            name: name.to_string(),
                            type_: PyType::from_str(type_)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                Ok(PyFunction {
                    name: func_name.to_string(),
                    args: typed_params,
                    return_type: PyType::from_str(return_type).unwrap(),
                    docstring: docstring.to_string(),
                })
            })
        })
        .collect()
}

/// Parse Python test code to extract test cases.
pub fn parse_test(
    parser: &mut Parser,
    src: &str,
    name: &str,
    return_type: &PyType,
) -> Result<Vec<Result<PyTest>>> {
    let tree = parser.parse(src, None).context("Could not parse")?;
    let root_node = tree.root_node();

    let query = tree_sitter::Query::new(
        tree_sitter_python::language(),
        "(assert_statement
 (comparison_operator) @assert)
",
    )?;
    let mut query_cursor = tree_sitter::QueryCursor::new();
    let matches = query_cursor.captures(&query, root_node, src.as_bytes());

    let tests = matches
        .flat_map(|(match_, _)| {
            match_.captures.iter().map(move |capture| {
                let assert_node = capture.node;

                // First child of comparison operator is the argument list
                let argument_list_result = assert_node
                    .named_child(0)
                    .and_then(|n| n.child_by_field_name("arguments"))
                    .ok_or_else(|| anyhow::anyhow!("No arguments found"))
                    .and_then(|args| {
                        args.named_children(&mut assert_node.walk())
                            .map(|n| PyValue::from_node(&n, src))
                            .collect::<Result<Vec<PyValue>>>()
                            .context("Could not parse arguments")
                    });

                // Second child is the result
                let result = assert_node
                    .named_child(1)
                    .map(|n| PyValue::from_node_and_type(&n, src, return_type))
                    .ok_or_else(|| anyhow::anyhow!("Could not get result"))
                    .and_then(|res| res.context("Could not parse result"));

                match (argument_list_result, result) {
                    (Ok(argument_list), Ok(result)) => Ok(PyTest {
                        name: name.to_string(),
                        inputs: argument_list,
                        output: result,
                    }),
                    (Err(e), _) | (_, Err(e)) => Err(e),
                }
            })
        })
        .collect();

    Ok(tests)
}

/// Parse the length of list arguments in test assertions.
pub fn parse_arg_list_length(parser: &mut Parser, src: &str) -> Option<usize> {
    let tree = parser.parse(src, None)?;
    let root_node = tree.root_node();

    let query = tree_sitter::Query::new(
        tree_sitter_python::language(),
        "(
        (assert_statement
            (comparison_operator
      (call
        function: (identifier)
        arguments: (argument_list
                    (list) @arg-list
                   )
      )
    )
  )
)",
    )
    .ok()?;
    let mut query_cursor = tree_sitter::QueryCursor::new();
    let mut matches = query_cursor.captures(&query, root_node, src.as_bytes());
    if let Some((m, _)) = matches.next() {
        if let Some(arg_list_node) = m.captures.iter().next() {
            let arg_list = arg_list_node.node;
            return Some(arg_list.named_child_count());
        }
    }
    None
}
