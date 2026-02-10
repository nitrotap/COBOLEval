"""Tests for utility functions."""

import os
import tempfile
from pathlib import Path

import pytest

from scripts.utils import Model, cleanup_file, cmd


class TestModel:
    """Tests for Model dataclass."""

    def test_model_defaults(self) -> None:
        """Test that Model has correct default values."""
        model = Model(name="test-model")

        assert model.name == "test-model"
        assert model.temp == 0.0
        assert model.samples_per_task == 1
        assert model.tokenizer is None
        assert model.prefix_token is None
        assert model.suffix_token is None
        assert model.middle_token is None
        assert model.eos_token is None

    def test_model_custom_values(self) -> None:
        """Test Model with custom values."""
        model = Model(
            name="gpt-4",
            temp=0.7,
            samples_per_task=5,
            tokenizer="custom-tokenizer",
        )

        assert model.name == "gpt-4"
        assert model.temp == 0.7
        assert model.samples_per_task == 5
        assert model.tokenizer == "custom-tokenizer"


class TestCmd:
    """Tests for cmd function."""

    def test_cmd_success(self) -> None:
        """Test that cmd returns True for successful commands."""
        result = cmd("echo 'hello'", timeout=5)
        assert result is True

    def test_cmd_failure(self) -> None:
        """Test that cmd returns False for failing commands."""
        result = cmd("exit 1", timeout=5)
        assert result is False

    def test_cmd_nonexistent_command(self) -> None:
        """Test that cmd returns False for non-existent commands."""
        result = cmd("nonexistent_command_12345", timeout=5)
        assert result is False


class TestCleanupFile:
    """Tests for cleanup_file function."""

    def test_cleanup_existing_file(self, tmp_path: Path) -> None:
        """Test that cleanup_file removes existing files."""
        test_file = tmp_path / "test.txt"
        test_file.write_text("test")

        assert test_file.exists()
        cleanup_file(str(test_file))
        assert not test_file.exists()

    def test_cleanup_nonexistent_file(self, tmp_path: Path) -> None:
        """Test that cleanup_file handles non-existent files gracefully."""
        test_file = tmp_path / "nonexistent.txt"

        # Should not raise an exception
        cleanup_file(str(test_file))
