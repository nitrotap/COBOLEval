"""JCL Validator - Validate JCL syntax and semantics."""

from dataclasses import dataclass, field
from typing import List, Optional, Set
from enum import Enum

from scripts.jcl.parser import JCLJob, JCLStatement, JCLStatementType, JCLStep


class ValidationSeverity(Enum):
    """Severity levels for validation messages."""
    ERROR = "ERROR"
    WARNING = "WARNING"
    INFO = "INFO"


@dataclass
class ValidationMessage:
    """A validation message."""
    severity: ValidationSeverity
    code: str
    message: str
    line_number: Optional[int] = None
    statement_name: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of JCL validation."""
    is_valid: bool
    messages: List[ValidationMessage] = field(default_factory=list)
    error_count: int = 0
    warning_count: int = 0

    def add_error(self, code: str, message: str, line: Optional[int] = None, name: Optional[str] = None) -> None:
        """Add an error message."""
        self.messages.append(ValidationMessage(
            severity=ValidationSeverity.ERROR,
            code=code,
            message=message,
            line_number=line,
            statement_name=name
        ))
        self.error_count += 1
        self.is_valid = False

    def add_warning(self, code: str, message: str, line: Optional[int] = None, name: Optional[str] = None) -> None:
        """Add a warning message."""
        self.messages.append(ValidationMessage(
            severity=ValidationSeverity.WARNING,
            code=code,
            message=message,
            line_number=line,
            statement_name=name
        ))
        self.warning_count += 1

    def add_info(self, code: str, message: str, line: Optional[int] = None, name: Optional[str] = None) -> None:
        """Add an info message."""
        self.messages.append(ValidationMessage(
            severity=ValidationSeverity.INFO,
            code=code,
            message=message,
            line_number=line,
            statement_name=name
        ))


class JCLValidator:
    """Validator for JCL jobs."""

    # Valid DD name characters
    VALID_NAME_CHARS = set("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#$")

    # Required DD names for common programs
    REQUIRED_DDS = {
        "SORT": ["SORTIN", "SORTOUT", "SYSIN"],
        "IEBGENER": ["SYSUT1", "SYSUT2", "SYSIN", "SYSPRINT"],
        "IEBCOPY": ["SYSUT1", "SYSUT2", "SYSIN", "SYSPRINT"],
        "IKJEFT01": ["SYSTSPRT", "SYSTSIN"],
        "IDCAMS": ["SYSPRINT", "SYSIN"],
    }

    # Valid JOB classes
    VALID_CLASSES = set("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

    def __init__(self) -> None:
        self.result = ValidationResult(is_valid=True)

    def validate(self, job: JCLJob) -> ValidationResult:
        """Validate a JCL job."""
        self.result = ValidationResult(is_valid=True)

        # Run all validations
        self._validate_job_statement(job)
        self._validate_steps(job)
        self._validate_dd_statements(job)
        self._validate_name_uniqueness(job)
        self._validate_references(job)

        return self.result

    def _validate_job_statement(self, job: JCLJob) -> None:
        """Validate the JOB statement."""
        # Check job name
        if not job.name or job.name == "UNKNOWN":
            self.result.add_error("JCL001", "JOB statement missing or invalid")
            return

        if len(job.name) > 8:
            self.result.add_error("JCL002", f"JOB name '{job.name}' exceeds 8 characters")

        if not self._is_valid_name(job.name):
            self.result.add_error("JCL003", f"JOB name '{job.name}' contains invalid characters")

        # Check CLASS
        if job.class_ and job.class_ not in self.VALID_CLASSES:
            self.result.add_warning("JCL004", f"Invalid CLASS '{job.class_}' specified")

        # Check MSGCLASS
        if job.msgclass and job.msgclass not in self.VALID_CLASSES:
            self.result.add_warning("JCL005", f"Invalid MSGCLASS '{job.msgclass}' specified")

    def _validate_steps(self, job: JCLJob) -> None:
        """Validate execution steps."""
        if not job.steps:
            self.result.add_warning("JCL010", "No EXEC steps found in job")
            return

        for i, step in enumerate(job.steps):
            # Check step name
            if step.name and len(step.name) > 8:
                self.result.add_error(
                    "JCL011",
                    f"Step name '{step.name}' exceeds 8 characters"
                )

            # Check that either PGM or PROC is specified
            if not step.program and not step.procedure:
                self.result.add_error(
                    "JCL012",
                    f"Step '{step.name}' has no PGM or PROC specified"
                )

            # Check for required DDs based on program
            if step.program:
                self._check_required_dds(step)

    def _validate_dd_statements(self, job: JCLJob) -> None:
        """Validate DD statements."""
        for stmt in job.statements:
            if stmt.statement_type != JCLStatementType.DD:
                continue

            # Check DD name
            if stmt.name:
                if len(stmt.name) > 8:
                    self.result.add_error(
                        "JCL020",
                        f"DD name '{stmt.name}' exceeds 8 characters",
                        stmt.line_number,
                        stmt.name
                    )

                if not self._is_valid_name(stmt.name):
                    self.result.add_error(
                        "JCL021",
                        f"DD name '{stmt.name}' contains invalid characters",
                        stmt.line_number,
                        stmt.name
                    )

            # Check for DSN or other data source
            operands = stmt.operands
            has_data_source = any(k in operands for k in ['DSN', 'DSNAME', '*', 'DATA', 'DUMMY', 'SYSOUT'])

            if not has_data_source and 'DDNAME' not in operands:
                self.result.add_warning(
                    "JCL022",
                    f"DD '{stmt.name}' has no data source specified",
                    stmt.line_number,
                    stmt.name
                )

            # Validate DISP parameter
            if 'DISP' in operands:
                self._validate_disp(operands['DISP'], stmt)

            # Validate SPACE parameter
            if 'SPACE' in operands:
                self._validate_space(operands['SPACE'], stmt)

    def _validate_name_uniqueness(self, job: JCLJob) -> None:
        """Check for duplicate names."""
        # Check step names
        step_names: Set[str] = set()
        for step in job.steps:
            if step.name in step_names:
                self.result.add_error(
                    "JCL030",
                    f"Duplicate step name '{step.name}'"
                )
            step_names.add(step.name)

        # Check DD names within each step
        for step in job.steps:
            dd_names: Set[str] = set()
            for dd in step.dd_statements:
                if dd.name and dd.name in dd_names:
                    self.result.add_warning(
                        "JCL031",
                        f"Duplicate DD name '{dd.name}' in step '{step.name}'",
                        dd.line_number,
                        dd.name
                    )
                if dd.name:
                    dd_names.add(dd.name)

    def _validate_references(self, job: JCLJob) -> None:
        """Validate backward references."""
        defined_dsns: Set[str] = set()

        for stmt in job.statements:
            if stmt.statement_type == JCLStatementType.DD:
                # Track defined datasets
                dsn = stmt.operands.get('DSN') or stmt.operands.get('DSNAME')
                if dsn and not dsn.startswith('&') and not dsn.startswith('*.'):
                    defined_dsns.add(dsn)

                # Check backward references
                if dsn and dsn.startswith('*.'):
                    ref_parts = dsn[2:].split('.')
                    ref_step = ref_parts[0] if ref_parts else None

                    # Verify the referenced step exists
                    if ref_step:
                        step_exists = any(s.name == ref_step for s in job.steps)
                        if not step_exists:
                            self.result.add_error(
                                "JCL040",
                                f"Backward reference to undefined step '{ref_step}'",
                                stmt.line_number,
                                stmt.name
                            )

    def _check_required_dds(self, step: JCLStep) -> None:
        """Check if required DDs are present for known programs."""
        program = step.program.upper() if step.program else None
        if program not in self.REQUIRED_DDS:
            return

        required = set(self.REQUIRED_DDS[program])
        present = {dd.name for dd in step.dd_statements if dd.name}
        missing = required - present

        for dd_name in missing:
            self.result.add_warning(
                "JCL050",
                f"Program '{program}' typically requires DD '{dd_name}' in step '{step.name}'"
            )

    def _validate_disp(self, disp: str, stmt: JCLStatement) -> None:
        """Validate DISP parameter."""
        valid_status = {'NEW', 'OLD', 'SHR', 'MOD'}
        valid_normal = {'DELETE', 'KEEP', 'PASS', 'CATLG', 'UNCATLG'}
        valid_abnormal = {'DELETE', 'KEEP', 'CATLG', 'UNCATLG'}

        # Parse DISP
        disp = disp.strip('()')
        parts = [p.strip() for p in disp.split(',')]

        if parts and parts[0] and parts[0].upper() not in valid_status:
            self.result.add_warning(
                "JCL060",
                f"Invalid DISP status '{parts[0]}'",
                stmt.line_number,
                stmt.name
            )

        if len(parts) > 1 and parts[1] and parts[1].upper() not in valid_normal:
            self.result.add_warning(
                "JCL061",
                f"Invalid DISP normal disposition '{parts[1]}'",
                stmt.line_number,
                stmt.name
            )

        if len(parts) > 2 and parts[2] and parts[2].upper() not in valid_abnormal:
            self.result.add_warning(
                "JCL062",
                f"Invalid DISP abnormal disposition '{parts[2]}'",
                stmt.line_number,
                stmt.name
            )

    def _validate_space(self, space: str, stmt: JCLStatement) -> None:
        """Validate SPACE parameter."""
        # Basic validation - check for valid unit
        valid_units = {'TRK', 'CYL', 'BLK'}
        space_upper = space.upper()

        has_valid_unit = any(unit in space_upper for unit in valid_units)
        if not has_valid_unit and not space_upper.startswith('('):
            self.result.add_warning(
                "JCL070",
                f"SPACE parameter may be invalid: '{space}'",
                stmt.line_number,
                stmt.name
            )

    def _is_valid_name(self, name: str) -> bool:
        """Check if a name contains only valid characters."""
        if not name:
            return False
        # First character must be alphabetic or national
        if name[0] not in "ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$":
            return False
        return all(c in self.VALID_NAME_CHARS for c in name.upper())


def validate_jcl(job: JCLJob) -> ValidationResult:
    """Convenience function to validate a JCL job."""
    validator = JCLValidator()
    return validator.validate(job)
