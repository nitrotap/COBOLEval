use crate::eval::types::{ParsedValue, PyType, SimpleType};
use anyhow::{anyhow, Result};

const FLOAT_TOLERANCE: f64 = 0.001;

/// Parse COBOL output lines according to the expected type.
/// Port of Python evaluation.py parse() function.
pub fn parse_result(output: &[String], type_: &PyType, expected_len: usize) -> Result<ParsedValue> {
    match type_ {
        PyType::Simple(SimpleType::Bool) => {
            let val = output
                .first()
                .ok_or_else(|| anyhow!("No output for Bool"))?;
            Ok(ParsedValue::Bool(parse_bool(val)))
        }
        PyType::Simple(SimpleType::Int) => {
            let val = output
                .first()
                .ok_or_else(|| anyhow!("No output for Int"))?;
            Ok(ParsedValue::Int(parse_int(val)?))
        }
        PyType::Simple(SimpleType::Float) => {
            let val = output
                .first()
                .ok_or_else(|| anyhow!("No output for Float"))?;
            Ok(ParsedValue::Float(parse_float(val)?))
        }
        PyType::Simple(SimpleType::String) => {
            let val = output
                .first()
                .ok_or_else(|| anyhow!("No output for String"))?;
            Ok(ParsedValue::Str(val.trim().to_string()))
        }
        PyType::List(map) => {
            let inner = map
                .get("List")
                .ok_or_else(|| anyhow!("Invalid List type"))?;
            match inner.as_str() {
                "Int" => {
                    let values: Vec<i64> = output
                        .iter()
                        .take(expected_len)
                        .map(|s| parse_int(s))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(ParsedValue::ListInt(values))
                }
                "Float" => {
                    let values: Vec<f64> = output
                        .iter()
                        .take(expected_len)
                        .map(|s| parse_float(s))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(ParsedValue::ListFloat(values))
                }
                "String" => {
                    let values: Vec<String> = output
                        .iter()
                        .take(expected_len)
                        .map(|s| s.trim().to_string())
                        .collect();
                    Ok(ParsedValue::ListStr(values))
                }
                _ => Err(anyhow!("Unknown list inner type: {}", inner)),
            }
        }
    }
}

/// Parse a COBOL boolean output: "1" = true, anything else = false
fn parse_bool(s: &str) -> bool {
    s.trim() == "1"
}

/// Parse a COBOL integer output.
/// Handles "p" or "y" prefix for negative numbers.
fn parse_int(s: &str) -> Result<i64> {
    let trimmed = s.trim();
    if trimmed.starts_with('p') || trimmed.starts_with('y') {
        let num: i64 = trimmed[1..].parse().map_err(|e| anyhow!("Parse int error: {}", e))?;
        Ok(-num)
    } else {
        trimmed
            .parse()
            .map_err(|e| anyhow!("Parse int error: {}", e))
    }
}

/// Parse a COBOL float output.
/// Handles "p" or "y" prefix for negative numbers.
fn parse_float(s: &str) -> Result<f64> {
    let trimmed = s.trim();
    if trimmed.starts_with('p') || trimmed.starts_with('y') {
        let num: f64 = trimmed[1..].parse().map_err(|e| anyhow!("Parse float error: {}", e))?;
        Ok(-num)
    } else {
        trimmed
            .parse()
            .map_err(|e| anyhow!("Parse float error: {}", e))
    }
}

/// Check if a parsed result matches the expected value.
/// Port of Python evaluation.py is_equal() function.
pub fn is_equal(type_: &PyType, result: &ParsedValue, expected: &ParsedValue) -> bool {
    match (type_, result, expected) {
        (PyType::Simple(SimpleType::Float), ParsedValue::Float(r), ParsedValue::Float(e)) => {
            (r - e).abs() < FLOAT_TOLERANCE
        }
        (PyType::List(map), ParsedValue::ListFloat(r), ParsedValue::ListFloat(e))
            if map.get("List").map(|s| s.as_str()) == Some("Float") =>
        {
            r.len() == e.len()
                && r.iter()
                    .zip(e.iter())
                    .all(|(a, b)| (a - b).abs() < FLOAT_TOLERANCE)
        }
        (_, ParsedValue::Bool(r), ParsedValue::Bool(e)) => r == e,
        (_, ParsedValue::Int(r), ParsedValue::Int(e)) => r == e,
        (_, ParsedValue::Str(r), ParsedValue::Str(e)) => r == e,
        (_, ParsedValue::ListInt(r), ParsedValue::ListInt(e)) => r == e,
        (_, ParsedValue::ListStr(r), ParsedValue::ListStr(e)) => r == e,
        _ => false,
    }
}

/// Parse an expected value string into a ParsedValue based on type.
/// The expected values come from CobolEval.jsonl result.value field.
pub fn parse_expected(value: &str, type_: &PyType) -> Result<ParsedValue> {
    match type_ {
        PyType::Simple(SimpleType::Bool) => {
            let b = value.trim() == "True";
            Ok(ParsedValue::Bool(b))
        }
        PyType::Simple(SimpleType::Int) => {
            let v: i64 = value.trim().parse().map_err(|e| anyhow!("Expected int: {}", e))?;
            Ok(ParsedValue::Int(v))
        }
        PyType::Simple(SimpleType::Float) => {
            let v: f64 = value.trim().parse().map_err(|e| anyhow!("Expected float: {}", e))?;
            Ok(ParsedValue::Float(v))
        }
        PyType::Simple(SimpleType::String) => Ok(ParsedValue::Str(value.trim().to_string())),
        PyType::List(map) => {
            let inner = map
                .get("List")
                .ok_or_else(|| anyhow!("Invalid List type"))?;
            // Expected values are Python list literals like "[1, 2, 3]" or "['a', 'b']"
            // We use Python's eval format — strip brackets, split by comma
            let stripped = value
                .trim()
                .trim_start_matches('[')
                .trim_end_matches(']');
            let items: Vec<&str> = stripped.split(',').map(|s| s.trim()).collect();

            match inner.as_str() {
                "Int" => {
                    let values: Vec<i64> = items
                        .iter()
                        .filter(|s| !s.is_empty())
                        .map(|s| {
                            s.trim()
                                .parse()
                                .map_err(|e| anyhow!("Expected list int: {}", e))
                        })
                        .collect::<Result<Vec<_>>>()?;
                    Ok(ParsedValue::ListInt(values))
                }
                "Float" => {
                    let values: Vec<f64> = items
                        .iter()
                        .filter(|s| !s.is_empty())
                        .map(|s| {
                            s.trim()
                                .parse()
                                .map_err(|e| anyhow!("Expected list float: {}", e))
                        })
                        .collect::<Result<Vec<_>>>()?;
                    Ok(ParsedValue::ListFloat(values))
                }
                "String" => {
                    let values: Vec<String> = items
                        .iter()
                        .filter(|s| !s.is_empty())
                        .map(|s| {
                            s.trim()
                                .trim_matches('\'')
                                .trim_matches('"')
                                .to_string()
                        })
                        .collect();
                    Ok(ParsedValue::ListStr(values))
                }
                _ => Err(anyhow!("Unknown list inner type: {}", inner)),
            }
        }
    }
}

/// Get the expected length of a list value from its string representation.
pub fn expected_list_len(value: &str) -> usize {
    let stripped = value
        .trim()
        .trim_start_matches('[')
        .trim_end_matches(']');
    if stripped.is_empty() {
        0
    } else {
        stripped.split(',').count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_parse_bool() {
        assert!(parse_bool("1"));
        assert!(!parse_bool("0"));
        assert!(!parse_bool("  0  "));
    }

    #[test]
    fn test_parse_int_positive() {
        assert_eq!(parse_int("42").unwrap(), 42);
        assert_eq!(parse_int("  42  ").unwrap(), 42);
    }

    #[test]
    fn test_parse_int_negative() {
        assert_eq!(parse_int("p5").unwrap(), -5);
        assert_eq!(parse_int("y10").unwrap(), -10);
    }

    #[test]
    fn test_parse_float() {
        assert!((parse_float("3.14").unwrap() - 3.14).abs() < 0.001);
        assert!((parse_float("p2.5").unwrap() - (-2.5)).abs() < 0.001);
    }

    #[test]
    fn test_is_equal_float() {
        let type_ = PyType::Simple(SimpleType::Float);
        assert!(is_equal(
            &type_,
            &ParsedValue::Float(3.1415),
            &ParsedValue::Float(3.1412)
        ));
        assert!(!is_equal(
            &type_,
            &ParsedValue::Float(3.14),
            &ParsedValue::Float(3.15)
        ));
    }

    #[test]
    fn test_parse_expected_list_int() {
        let mut map = HashMap::new();
        map.insert("List".to_string(), "Int".to_string());
        let type_ = PyType::List(map);
        let result = parse_expected("[1, 2, 3]", &type_).unwrap();
        match result {
            ParsedValue::ListInt(v) => assert_eq!(v, vec![1, 2, 3]),
            _ => panic!("Expected ListInt"),
        }
    }

    #[test]
    fn test_expected_list_len() {
        assert_eq!(expected_list_len("[1, 2, 3]"), 3);
        assert_eq!(expected_list_len("['a', 'b']"), 2);
        assert_eq!(expected_list_len("[]"), 0);
    }
}
