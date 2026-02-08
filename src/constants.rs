//! Constants used throughout the COBOLEval transpiler.

/// Characters used for indexing COBOL arrays (INDEXED BY N{char}).
pub const INDEX_CHARS: &[&str] = &["I", "J", "K", "L", "M"];

/// Default size for COBOL arrays.
#[allow(dead_code)]
pub const DEFAULT_ARRAY_SIZE: usize = 100;

/// Default string field width in COBOL.
#[allow(dead_code)]
pub const DEFAULT_STRING_WIDTH: usize = 100;

/// Default integer picture clause.
pub const INT_PIC: &str = "PIC S9(10)";

/// Default float type in COBOL.
pub const FLOAT_TYPE: &str = "COMP-2";

/// Default boolean picture clause (single digit).
pub const BOOL_PIC: &str = "PIC 9";

/// Default string picture clause.
pub const STRING_PIC: &str = "PIC X(100)";
