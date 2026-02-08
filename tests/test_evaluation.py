"""Tests for evaluation functions."""

import math

import pytest

from scripts.evaluation import (
    ParseError,
    estimate_pass_at_k,
    find_index_or_last,
    is_equal,
    parse_bool,
    parse_float,
    parse_int,
    parse_string,
)


class TestParseBool:
    """Tests for parse_bool function."""

    def test_parse_true(self) -> None:
        """Test parsing '1' as True."""
        assert parse_bool("1") is True
        assert parse_bool("  1  ") is True

    def test_parse_false(self) -> None:
        """Test parsing other values as False."""
        assert parse_bool("0") is False
        assert parse_bool("") is False
        assert parse_bool("false") is False


class TestParseInt:
    """Tests for parse_int function."""

    def test_parse_positive_int(self) -> None:
        """Test parsing positive integers."""
        assert parse_int("42") == 42
        assert parse_int("  100  ") == 100

    def test_parse_negative_int_p_prefix(self) -> None:
        """Test parsing negative integers with 'p' prefix."""
        assert parse_int("p42") == -42
        assert parse_int("p100") == -100

    def test_parse_negative_int_y_prefix(self) -> None:
        """Test parsing negative integers with 'y' prefix."""
        assert parse_int("y42") == -42
        assert parse_int("y100") == -100


class TestParseFloat:
    """Tests for parse_float function."""

    def test_parse_positive_float(self) -> None:
        """Test parsing positive floats."""
        assert parse_float("3.14") == pytest.approx(3.14)
        assert parse_float("  2.5  ") == pytest.approx(2.5)

    def test_parse_negative_float_p_prefix(self) -> None:
        """Test parsing negative floats with 'p' prefix."""
        assert parse_float("p3.14") == pytest.approx(-3.14)

    def test_parse_negative_float_y_prefix(self) -> None:
        """Test parsing negative floats with 'y' prefix."""
        assert parse_float("y2.5") == pytest.approx(-2.5)


class TestParseString:
    """Tests for parse_string function."""

    def test_parse_string_strips_whitespace(self) -> None:
        """Test that parse_string strips whitespace."""
        assert parse_string("  hello  ") == "hello"
        assert parse_string("world") == "world"


class TestFindIndexOrLast:
    """Tests for find_index_or_last function."""

    def test_find_existing_element(self) -> None:
        """Test finding an existing element."""
        assert find_index_or_last([1, 2, 3], 2) == 1
        assert find_index_or_last(["a", "b", "c"], "b") == 1

    def test_find_nonexistent_element(self) -> None:
        """Test that non-existent element returns last index."""
        assert find_index_or_last([1, 2, 3], 5) == 2
        assert find_index_or_last(["a", "b", "c"], "z") == 2


class TestIsEqual:
    """Tests for is_equal function."""

    def test_int_equality(self) -> None:
        """Test integer equality."""
        assert is_equal("Int", 42, 42) is True
        assert is_equal("Int", 42, 43) is False

    def test_float_equality_with_tolerance(self) -> None:
        """Test float equality with tolerance."""
        assert is_equal("Float", 3.14159, 3.14159) is True
        assert is_equal("Float", 3.14159, 3.1416) is True  # Within tolerance
        assert is_equal("Float", 3.14159, 3.15) is False  # Outside tolerance

    def test_string_equality(self) -> None:
        """Test string equality."""
        assert is_equal("String", "hello", "hello") is True
        assert is_equal("String", "hello", "world") is False

    def test_list_float_equality(self) -> None:
        """Test list of floats equality."""
        assert is_equal({"List": "Float"}, [1.0, 2.0], [1.0, 2.0]) is True
        assert is_equal({"List": "Float"}, [1.0, 2.0], [1.001, 2.001]) is True


class TestEstimatePassAtK:
    """Tests for estimate_pass_at_k function."""

    def test_pass_at_1_all_correct(self) -> None:
        """Test pass@1 when all samples are correct."""
        result = estimate_pass_at_k(10, [10], 1)
        assert result[0] == pytest.approx(1.0)

    def test_pass_at_1_none_correct(self) -> None:
        """Test pass@1 when no samples are correct."""
        result = estimate_pass_at_k(10, [0], 1)
        assert result[0] == pytest.approx(0.0)

    def test_pass_at_1_half_correct(self) -> None:
        """Test pass@1 when half of samples are correct."""
        result = estimate_pass_at_k(10, [5], 1)
        assert result[0] == pytest.approx(0.5)

    def test_pass_at_k_insufficient_samples(self) -> None:
        """Test pass@k returns 1.0 when n-c < k."""
        result = estimate_pass_at_k(10, [9], 5)
        assert result[0] == pytest.approx(1.0)
