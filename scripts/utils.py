"""Utility functions and data classes for COBOLEval."""

import os
import subprocess
from dataclasses import dataclass
from typing import Optional

from loguru import logger


# Default timeout for subprocess commands (in seconds)
DEFAULT_TIMEOUT: int = int(os.environ.get("COBOLEVAL_TIMEOUT", "30"))


@dataclass
class Model:
    """Configuration for an LLM model."""

    name: str
    temp: float = 0.0
    samples_per_task: int = 1
    tokenizer: Optional[str] = None
    prefix_token: Optional[str] = None
    suffix_token: Optional[str] = None
    middle_token: Optional[str] = None
    eos_token: Optional[str] = None


def cmd(command: str, timeout: int = DEFAULT_TIMEOUT) -> bool:
    """
    Execute a shell command with timeout.

    Args:
        command: The shell command to execute
        timeout: Timeout in seconds (default from COBOLEVAL_TIMEOUT env var or 30s)

    Returns:
        True if command succeeded (return code 0), False otherwise
    """
    try:
        process = subprocess.run(
            command,
            shell=True,
            text=True,
            capture_output=True,
            timeout=timeout,
        )

        if process.stderr:
            logger.warning(f"Err: {process.stderr}")
            logger.warning(f"Return code: {process.returncode}")
        return process.returncode == 0
    except subprocess.TimeoutExpired:
        logger.warning(f"Command timed out after {timeout}s: {command}")
        return False
    except subprocess.SubprocessError as e:
        logger.error(f"Subprocess error: {e}")
        return False


def cleanup_dylib(name: str) -> None:
    """Remove a dynamic library file (macOS .dylib)."""
    try:
        os.remove(f"{name}.dylib")
    except FileNotFoundError:
        logger.debug(f"File {name}.dylib not found (may already be cleaned up)")
    except OSError as e:
        logger.warning(f"Error removing {name}.dylib: {e}")


def cleanup_file(name: str) -> None:
    """Remove a file by name."""
    try:
        os.remove(name)
    except FileNotFoundError:
        logger.debug(f"File {name} not found (may already be cleaned up)")
    except OSError as e:
        logger.warning(f"Error removing {name}: {e}")
