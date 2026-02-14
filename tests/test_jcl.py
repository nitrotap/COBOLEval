"""Tests for JCL parser and validator."""

import pytest

from scripts.jcl.parser import JCLParser, JCLStatementType, parse_jcl
from scripts.jcl.validator import JCLValidator, ValidationSeverity, validate_jcl


class TestJCLParser:
    """Tests for JCL parser."""

    def test_parse_simple_job(self) -> None:
        """Test parsing a simple JCL job."""
        jcl = """//MYJOB    JOB (ACCT),'TEST JOB',CLASS=A
//STEP1    EXEC PGM=IEFBR14
//DD1      DD DUMMY
"""
        job = parse_jcl(jcl)

        assert job.name == "MYJOB"
        assert len(job.steps) == 1
        assert job.steps[0].name == "STEP1"
        assert job.steps[0].program == "IEFBR14"

    def test_parse_job_with_multiple_steps(self) -> None:
        """Test parsing a job with multiple steps."""
        jcl = """//MULTISTEP JOB (ACCT),'MULTI STEP',CLASS=A
//STEP1    EXEC PGM=PROG1
//INPUT    DD DSN=TEST.FILE,DISP=SHR
//STEP2    EXEC PGM=PROG2
//OUTPUT   DD DSN=TEST.OUT,DISP=(NEW,CATLG)
"""
        job = parse_jcl(jcl)

        assert job.name == "MULTISTEP"
        assert len(job.steps) == 2
        assert job.steps[0].program == "PROG1"
        assert job.steps[1].program == "PROG2"

    def test_parse_comments(self) -> None:
        """Test that comments are parsed correctly."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//* This is a comment
//STEP1    EXEC PGM=TEST
"""
        job = parse_jcl(jcl)

        comment_stmts = [s for s in job.statements if s.statement_type == JCLStatementType.COMMENT]
        assert len(comment_stmts) == 1
        assert "This is a comment" in comment_stmts[0].comments

    def test_parse_dd_with_dsn(self) -> None:
        """Test parsing DD statement with DSN."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC PGM=TEST
//INFILE   DD DSN=MY.DATASET.NAME,DISP=SHR
"""
        job = parse_jcl(jcl)

        dd_stmts = [s for s in job.statements if s.statement_type == JCLStatementType.DD]
        assert len(dd_stmts) == 1
        assert dd_stmts[0].operands.get('DSN') == 'MY.DATASET.NAME'
        assert dd_stmts[0].operands.get('DISP') == 'SHR'

    def test_parse_exec_with_proc(self) -> None:
        """Test parsing EXEC with procedure call."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC MYPROC,PARM='TEST'
"""
        job = parse_jcl(jcl)

        assert len(job.steps) == 1
        assert job.steps[0].procedure == "MYPROC"

    def test_parse_sysout(self) -> None:
        """Test parsing SYSOUT DD."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC PGM=TEST
//SYSPRINT DD SYSOUT=*
"""
        job = parse_jcl(jcl)

        dd_stmts = [s for s in job.statements if s.statement_type == JCLStatementType.DD]
        assert len(dd_stmts) == 1
        assert dd_stmts[0].operands.get('SYSOUT') == '*'


class TestJCLValidator:
    """Tests for JCL validator."""

    def test_validate_valid_job(self) -> None:
        """Test validation of a valid JCL job."""
        jcl = """//VALIDJOB JOB (ACCT),'VALID JOB',CLASS=A
//STEP1    EXEC PGM=IEFBR14
//DD1      DD DUMMY
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        assert result.is_valid
        assert result.error_count == 0

    def test_validate_missing_job(self) -> None:
        """Test validation when JOB statement is missing."""
        jcl = """//STEP1    EXEC PGM=IEFBR14
//DD1      DD DUMMY
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        assert not result.is_valid
        assert result.error_count > 0

    def test_validate_long_job_name(self) -> None:
        """Test validation of job name exceeding 8 characters."""
        jcl = """//TOOLONGJOBNAME JOB (ACCT),'TEST'
//STEP1    EXEC PGM=TEST
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        assert not result.is_valid
        error_codes = [m.code for m in result.messages if m.severity == ValidationSeverity.ERROR]
        assert "JCL002" in error_codes

    def test_validate_missing_pgm(self) -> None:
        """Test validation when EXEC has no PGM or PROC."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        assert not result.is_valid
        error_codes = [m.code for m in result.messages if m.severity == ValidationSeverity.ERROR]
        assert "JCL012" in error_codes

    def test_validate_invalid_disp(self) -> None:
        """Test validation of invalid DISP parameter."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC PGM=TEST
//DD1      DD DSN=TEST.FILE,DISP=(INVALID,KEEP)
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        warning_codes = [m.code for m in result.messages if m.severity == ValidationSeverity.WARNING]
        assert "JCL060" in warning_codes

    def test_validate_duplicate_step_names(self) -> None:
        """Test validation of duplicate step names."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC PGM=PROG1
//DD1      DD DUMMY
//STEP1    EXEC PGM=PROG2
//DD2      DD DUMMY
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        assert not result.is_valid
        error_codes = [m.code for m in result.messages if m.severity == ValidationSeverity.ERROR]
        assert "JCL030" in error_codes

    def test_validate_backward_reference(self) -> None:
        """Test validation of backward references."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC PGM=PROG1
//OUTPUT   DD DSN=WORK.FILE,DISP=(NEW,PASS)
//STEP2    EXEC PGM=PROG2
//INPUT    DD DSN=*.STEP1.OUTPUT,DISP=SHR
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        # Valid backward reference should not produce errors
        error_codes = [m.code for m in result.messages if m.severity == ValidationSeverity.ERROR]
        assert "JCL040" not in error_codes

    def test_validate_invalid_backward_reference(self) -> None:
        """Test validation of invalid backward reference."""
        jcl = """//TESTJOB  JOB (ACCT),'TEST'
//STEP1    EXEC PGM=PROG1
//OUTPUT   DD DSN=WORK.FILE,DISP=(NEW,PASS)
//STEP2    EXEC PGM=PROG2
//INPUT    DD DSN=*.NOSTEP.OUTPUT,DISP=SHR
"""
        job = parse_jcl(jcl)
        result = validate_jcl(job)

        error_codes = [m.code for m in result.messages if m.severity == ValidationSeverity.ERROR]
        assert "JCL040" in error_codes
