"""JCL Parser - Parse and validate JCL syntax."""

import re
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple
from enum import Enum


class JCLStatementType(Enum):
    """Types of JCL statements."""
    JOB = "JOB"
    EXEC = "EXEC"
    DD = "DD"
    PROC = "PROC"
    PEND = "PEND"
    SET = "SET"
    IF = "IF"
    ELSE = "ELSE"
    ENDIF = "ENDIF"
    INCLUDE = "INCLUDE"
    JCLLIB = "JCLLIB"
    OUTPUT = "OUTPUT"
    COMMENT = "COMMENT"
    NULL = "NULL"
    DELIMITER = "DELIMITER"
    UNKNOWN = "UNKNOWN"


@dataclass
class JCLStatement:
    """A parsed JCL statement."""
    line_number: int
    raw_text: str
    statement_type: JCLStatementType
    name: Optional[str] = None
    operation: Optional[str] = None
    operands: Dict[str, str] = field(default_factory=dict)
    comments: str = ""
    continuation: bool = False
    errors: List[str] = field(default_factory=list)


@dataclass
class JCLStep:
    """A JCL execution step (EXEC statement with associated DDs)."""
    name: str
    program: Optional[str] = None
    procedure: Optional[str] = None
    dd_statements: List[JCLStatement] = field(default_factory=list)
    parameters: Dict[str, str] = field(default_factory=dict)


@dataclass
class JCLJob:
    """A complete JCL job."""
    name: str
    accounting: str = ""
    programmer: str = ""
    class_: str = "A"
    msgclass: str = "A"
    msglevel: Tuple[int, int] = (1, 1)
    steps: List[JCLStep] = field(default_factory=list)
    statements: List[JCLStatement] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)


class JCLParser:
    """Parser for IBM JCL (Job Control Language)."""

    # JCL statement patterns
    # Allow any length name (validator will check for >8 chars)
    STATEMENT_PATTERN = re.compile(
        r'^//(\S*)\s+(\S+)\s*(.*)?$'
    )
    COMMENT_PATTERN = re.compile(r'^//\*(.*)$')
    CONTINUATION_PATTERN = re.compile(r'^//\s{9,}(.*)$')
    DELIMITER_PATTERN = re.compile(r'^/\*(.*)$')
    NULL_PATTERN = re.compile(r'^//\s*$')

    # Operand parsing
    KEYWORD_PATTERN = re.compile(r"(\w+)=([^,]+|'[^']*'|\([^)]*\))")

    def __init__(self) -> None:
        self.errors: List[str] = []
        self.warnings: List[str] = []

    def parse(self, jcl_text: str) -> JCLJob:
        """Parse JCL text and return a JCLJob object."""
        self.errors = []
        self.warnings = []

        lines = jcl_text.split('\n')
        statements = self._parse_statements(lines)

        # Find JOB statement
        job_stmt = next(
            (s for s in statements if s.statement_type == JCLStatementType.JOB),
            None
        )

        if not job_stmt:
            self.errors.append("No JOB statement found")
            return JCLJob(name="UNKNOWN", statements=statements, errors=self.errors)

        job = JCLJob(
            name=job_stmt.name or "UNKNOWN",
            statements=statements,
            errors=self.errors,
            warnings=self.warnings
        )

        # Parse JOB parameters
        job.class_ = job_stmt.operands.get("CLASS", "A")
        job.msgclass = job_stmt.operands.get("MSGCLASS", "A")

        # Extract steps
        job.steps = self._extract_steps(statements)

        return job

    def _parse_statements(self, lines: List[str]) -> List[JCLStatement]:
        """Parse all lines into JCL statements."""
        statements = []
        current_statement = None

        for i, line in enumerate(lines, 1):
            # Skip empty lines
            if not line.strip():
                continue

            # Check for comment
            comment_match = self.COMMENT_PATTERN.match(line)
            if comment_match:
                statements.append(JCLStatement(
                    line_number=i,
                    raw_text=line,
                    statement_type=JCLStatementType.COMMENT,
                    comments=comment_match.group(1).strip()
                ))
                continue

            # Check for delimiter
            delim_match = self.DELIMITER_PATTERN.match(line)
            if delim_match:
                statements.append(JCLStatement(
                    line_number=i,
                    raw_text=line,
                    statement_type=JCLStatementType.DELIMITER
                ))
                continue

            # Check for null statement
            if self.NULL_PATTERN.match(line):
                statements.append(JCLStatement(
                    line_number=i,
                    raw_text=line,
                    statement_type=JCLStatementType.NULL
                ))
                continue

            # Check for continuation
            cont_match = self.CONTINUATION_PATTERN.match(line)
            if cont_match and current_statement:
                current_statement.raw_text += '\n' + line
                self._parse_operands(cont_match.group(1), current_statement)
                continue

            # Parse regular statement
            stmt_match = self.STATEMENT_PATTERN.match(line)
            if stmt_match:
                name = stmt_match.group(1) or None
                operation = stmt_match.group(2).upper()
                operand_text = stmt_match.group(3) or ""

                stmt_type = self._get_statement_type(operation)

                current_statement = JCLStatement(
                    line_number=i,
                    raw_text=line,
                    statement_type=stmt_type,
                    name=name,
                    operation=operation,
                    continuation=operand_text.rstrip().endswith(',')
                )

                self._parse_operands(operand_text, current_statement)
                statements.append(current_statement)
            else:
                # Invalid statement
                statements.append(JCLStatement(
                    line_number=i,
                    raw_text=line,
                    statement_type=JCLStatementType.UNKNOWN,
                    errors=[f"Invalid JCL statement format at line {i}"]
                ))

        return statements

    def _get_statement_type(self, operation: str) -> JCLStatementType:
        """Determine the statement type from the operation."""
        type_map = {
            "JOB": JCLStatementType.JOB,
            "EXEC": JCLStatementType.EXEC,
            "DD": JCLStatementType.DD,
            "PROC": JCLStatementType.PROC,
            "PEND": JCLStatementType.PEND,
            "SET": JCLStatementType.SET,
            "IF": JCLStatementType.IF,
            "ELSE": JCLStatementType.ELSE,
            "ENDIF": JCLStatementType.ENDIF,
            "INCLUDE": JCLStatementType.INCLUDE,
            "JCLLIB": JCLStatementType.JCLLIB,
            "OUTPUT": JCLStatementType.OUTPUT,
        }
        return type_map.get(operation, JCLStatementType.UNKNOWN)

    def _parse_operands(self, operand_text: str, statement: JCLStatement) -> None:
        """Parse operands from operand text."""
        # Remove trailing comma and comments
        operand_text = operand_text.split('//')[0].strip().rstrip(',')

        # Find keyword parameters
        for match in self.KEYWORD_PATTERN.finditer(operand_text):
            key = match.group(1).upper()
            value = match.group(2).strip("'")
            statement.operands[key] = value

        # Handle positional parameters for specific statements
        if statement.statement_type == JCLStatementType.EXEC:
            # First positional is PGM= or proc name
            first_part = operand_text.split(',')[0].strip()
            if '=' not in first_part:
                statement.operands['PROC'] = first_part
            elif first_part.upper().startswith('PGM='):
                statement.operands['PGM'] = first_part[4:]

    def _extract_steps(self, statements: List[JCLStatement]) -> List[JCLStep]:
        """Extract execution steps from statements."""
        steps = []
        current_step = None

        for stmt in statements:
            if stmt.statement_type == JCLStatementType.EXEC:
                if current_step:
                    steps.append(current_step)

                current_step = JCLStep(
                    name=stmt.name or f"STEP{len(steps)+1:03d}",
                    program=stmt.operands.get('PGM'),
                    procedure=stmt.operands.get('PROC'),
                    parameters=stmt.operands
                )
            elif stmt.statement_type == JCLStatementType.DD and current_step:
                current_step.dd_statements.append(stmt)

        if current_step:
            steps.append(current_step)

        return steps


def parse_jcl(jcl_text: str) -> JCLJob:
    """Convenience function to parse JCL text."""
    parser = JCLParser()
    return parser.parse(jcl_text)
