"""JCL Evaluation CLI - Command-line interface for JCL tools."""

import json
import os
import sys
from typing import Optional

import fire
from loguru import logger

from scripts.jcl.parser import JCLParser, JCLJob, parse_jcl
from scripts.jcl.validator import JCLValidator, ValidationResult, ValidationSeverity, validate_jcl


def parse(jcl_path: str, output_format: str = "text") -> None:
    """
    Parse a JCL file and display its structure.

    Args:
        jcl_path: Path to the JCL file
        output_format: Output format - "text" or "json"
    """
    if not os.path.exists(jcl_path):
        logger.error(f"File not found: {jcl_path}")
        sys.exit(1)

    with open(jcl_path, 'r') as f:
        jcl_text = f.read()

    job = parse_jcl(jcl_text)

    if output_format == "json":
        _print_job_json(job)
    else:
        _print_job_text(job)


def validate(jcl_path: str, strict: bool = False) -> int:
    """
    Validate a JCL file for syntax and semantic errors.

    Args:
        jcl_path: Path to the JCL file
        strict: Treat warnings as errors

    Returns:
        Exit code (0 = valid, 1 = errors, 2 = warnings with strict mode)
    """
    if not os.path.exists(jcl_path):
        logger.error(f"File not found: {jcl_path}")
        return 1

    with open(jcl_path, 'r') as f:
        jcl_text = f.read()

    job = parse_jcl(jcl_text)
    result = validate_jcl(job)

    _print_validation_result(result, jcl_path)

    if result.error_count > 0:
        return 1
    if strict and result.warning_count > 0:
        return 2
    return 0


def lint(jcl_path: str, fix: bool = False) -> None:
    """
    Lint a JCL file and suggest improvements.

    Args:
        jcl_path: Path to the JCL file
        fix: Attempt to auto-fix issues (not yet implemented)
    """
    if not os.path.exists(jcl_path):
        logger.error(f"File not found: {jcl_path}")
        sys.exit(1)

    with open(jcl_path, 'r') as f:
        jcl_text = f.read()

    job = parse_jcl(jcl_text)
    result = validate_jcl(job)

    # Additional lint checks
    _lint_job(job, result)

    _print_validation_result(result, jcl_path)

    if fix:
        logger.warning("Auto-fix not yet implemented")


def info(jcl_path: str) -> None:
    """
    Display summary information about a JCL file.

    Args:
        jcl_path: Path to the JCL file
    """
    if not os.path.exists(jcl_path):
        logger.error(f"File not found: {jcl_path}")
        sys.exit(1)

    with open(jcl_path, 'r') as f:
        jcl_text = f.read()

    job = parse_jcl(jcl_text)

    print(f"\n{'='*60}")
    print(f"JCL Job Summary: {jcl_path}")
    print(f"{'='*60}")
    print(f"Job Name:     {job.name}")
    print(f"Class:        {job.class_}")
    print(f"Msgclass:     {job.msgclass}")
    print(f"Total Steps:  {len(job.steps)}")
    print(f"Statements:   {len(job.statements)}")

    if job.steps:
        print(f"\n{'Steps':}")
        print(f"{'-'*40}")
        for i, step in enumerate(job.steps, 1):
            pgm_or_proc = step.program or step.procedure or "N/A"
            dd_count = len(step.dd_statements)
            print(f"  {i}. {step.name:8} -> {pgm_or_proc:12} ({dd_count} DDs)")

    # Validate and show summary
    result = validate_jcl(job)
    print(f"\n{'Validation':}")
    print(f"{'-'*40}")
    print(f"  Errors:   {result.error_count}")
    print(f"  Warnings: {result.warning_count}")
    print(f"  Status:   {'VALID' if result.is_valid else 'INVALID'}")
    print()


def batch_validate(directory: str, pattern: str = "*.jcl") -> int:
    """
    Validate all JCL files in a directory.

    Args:
        directory: Directory containing JCL files
        pattern: Glob pattern for JCL files

    Returns:
        Number of invalid files
    """
    import glob

    if not os.path.isdir(directory):
        logger.error(f"Directory not found: {directory}")
        return -1

    files = glob.glob(os.path.join(directory, pattern))
    if not files:
        logger.warning(f"No files matching '{pattern}' found in {directory}")
        return 0

    total = len(files)
    valid = 0
    invalid = 0
    warnings = 0

    print(f"\nValidating {total} JCL files in {directory}")
    print("=" * 60)

    for jcl_path in sorted(files):
        with open(jcl_path, 'r') as f:
            jcl_text = f.read()

        job = parse_jcl(jcl_text)
        result = validate_jcl(job)

        filename = os.path.basename(jcl_path)
        if result.is_valid:
            if result.warning_count > 0:
                print(f"  WARN  {filename} ({result.warning_count} warnings)")
                warnings += 1
            else:
                print(f"  OK    {filename}")
            valid += 1
        else:
            print(f"  FAIL  {filename} ({result.error_count} errors)")
            invalid += 1

    print("=" * 60)
    print(f"Results: {valid} valid, {invalid} invalid, {warnings} with warnings")
    print()

    return invalid


def _print_job_text(job: JCLJob) -> None:
    """Print job structure in text format."""
    print(f"\n{'='*60}")
    print(f"JCL Job: {job.name}")
    print(f"{'='*60}")

    for stmt in job.statements:
        prefix = f"[{stmt.line_number:3d}]"
        if stmt.statement_type.value == "COMMENT":
            print(f"{prefix} // *{stmt.comments}")
        elif stmt.name:
            print(f"{prefix} //{stmt.name:8} {stmt.operation or ''}")
            if stmt.operands:
                for k, v in stmt.operands.items():
                    print(f"          {k}={v}")
        else:
            print(f"{prefix} //         {stmt.operation or ''}")

    print()


def _print_job_json(job: JCLJob) -> None:
    """Print job structure in JSON format."""
    data = {
        "name": job.name,
        "class": job.class_,
        "msgclass": job.msgclass,
        "steps": [
            {
                "name": step.name,
                "program": step.program,
                "procedure": step.procedure,
                "dd_count": len(step.dd_statements),
                "parameters": step.parameters
            }
            for step in job.steps
        ],
        "statement_count": len(job.statements),
        "errors": job.errors
    }
    print(json.dumps(data, indent=2))


def _print_validation_result(result: ValidationResult, filename: str) -> None:
    """Print validation results."""
    print(f"\n{'='*60}")
    print(f"Validation Results: {filename}")
    print(f"{'='*60}")

    if not result.messages:
        print("  No issues found.")
    else:
        for msg in result.messages:
            severity = msg.severity.value
            location = f"Line {msg.line_number}" if msg.line_number else ""
            if msg.statement_name:
                location = f"{location} ({msg.statement_name})" if location else msg.statement_name

            print(f"  [{severity:7}] {msg.code}: {msg.message}")
            if location:
                print(f"           at {location}")

    print(f"\n  Summary: {result.error_count} errors, {result.warning_count} warnings")
    print(f"  Status:  {'VALID' if result.is_valid else 'INVALID'}")
    print()


def _lint_job(job: JCLJob, result: ValidationResult) -> None:
    """Additional lint checks beyond validation."""
    # Check for best practices
    for step in job.steps:
        # Suggest REGION parameter
        if 'REGION' not in step.parameters:
            result.add_info(
                "LINT001",
                f"Consider adding REGION parameter to step '{step.name}'"
            )

        # Check for COND/IF usage
        if 'COND' not in step.parameters:
            result.add_info(
                "LINT002",
                f"Consider adding COND parameter for conditional execution in step '{step.name}'"
            )

    # Check for NOTIFY
    job_stmt = next(
        (s for s in job.statements if s.operation == "JOB"),
        None
    )
    if job_stmt and 'NOTIFY' not in job_stmt.operands:
        result.add_info(
            "LINT003",
            "Consider adding NOTIFY parameter for job completion notification"
        )


def main() -> None:
    """CLI entry point for JCL tools."""
    fire.Fire({
        "parse": parse,
        "validate": validate,
        "lint": lint,
        "info": info,
        "batch": batch_validate,
    })


if __name__ == "__main__":
    main()
