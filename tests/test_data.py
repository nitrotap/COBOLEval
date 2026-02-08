"""Tests for data loading utilities."""

import json
import tempfile
from pathlib import Path

import pytest

from scripts.data import read_problems, stream_jsonl, write_jsonl


class TestStreamJsonl:
    """Tests for stream_jsonl function."""

    def test_stream_jsonl_reads_lines(self, tmp_path: Path) -> None:
        """Test that stream_jsonl correctly reads JSONL files."""
        test_file = tmp_path / "test.jsonl"
        test_data = [{"id": 1, "name": "test1"}, {"id": 2, "name": "test2"}]

        with open(test_file, "w") as f:
            for item in test_data:
                f.write(json.dumps(item) + "\n")

        result = list(stream_jsonl(str(test_file)))
        assert len(result) == 2
        assert result[0]["id"] == 1
        assert result[1]["name"] == "test2"

    def test_stream_jsonl_skips_empty_lines(self, tmp_path: Path) -> None:
        """Test that empty lines are skipped."""
        test_file = tmp_path / "test.jsonl"

        with open(test_file, "w") as f:
            f.write('{"id": 1}\n')
            f.write("\n")
            f.write("   \n")
            f.write('{"id": 2}\n')

        result = list(stream_jsonl(str(test_file)))
        assert len(result) == 2


class TestWriteJsonl:
    """Tests for write_jsonl function."""

    def test_write_jsonl_creates_file(self, tmp_path: Path) -> None:
        """Test that write_jsonl creates a JSONL file."""
        test_file = tmp_path / "output.jsonl"
        test_data = [{"id": 1}, {"id": 2}]

        write_jsonl(str(test_file), test_data)

        assert test_file.exists()
        result = list(stream_jsonl(str(test_file)))
        assert len(result) == 2

    def test_write_jsonl_append_mode(self, tmp_path: Path) -> None:
        """Test that append mode works correctly."""
        test_file = tmp_path / "output.jsonl"

        write_jsonl(str(test_file), [{"id": 1}])
        write_jsonl(str(test_file), [{"id": 2}], append=True)

        result = list(stream_jsonl(str(test_file)))
        assert len(result) == 2


class TestReadProblems:
    """Tests for read_problems function."""

    def test_read_problems_returns_dict(self, tmp_path: Path, monkeypatch) -> None:
        """Test that read_problems returns a dictionary keyed by task_id."""
        test_file = tmp_path / "problems.jsonl"
        test_data = [
            {"task_id": "task/1", "prompt": "test1"},
            {"task_id": "task/2", "prompt": "test2"},
        ]

        with open(test_file, "w") as f:
            for item in test_data:
                f.write(json.dumps(item) + "\n")

        result = read_problems(str(test_file))

        assert isinstance(result, dict)
        assert "task/1" in result
        assert "task/2" in result
        assert result["task/1"]["prompt"] == "test1"
