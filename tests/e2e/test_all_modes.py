#!/usr/bin/env python3
"""
E2E Tests for Hybrid Architecture Template
===========================================
Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
SPDX-License-Identifier: BSD-3-Clause

Purpose:
    End-to-end tests for both synchronous and concurrent execution modes.
    Validates that both implementations honor the same CLI contract.

    Demonstrates architectural principle:
    "Good architecture lets you swap implementations. E2E tests prove it."

Tests:
    - Clean startup and shutdown (no hanging)
    - Error handling and exit codes
    - Signal handler integration
    - Result type error propagation
    - Behavioral equivalence across execution modes

Usage:
    # Run all tests
    python3 -m pytest tests/e2e/test_all_modes.py -v

    # Run specific test
    python3 -m pytest tests/e2e/test_all_modes.py::TestCLIContract::test_help -v

    # Run with verbose output
    python3 -m pytest tests/e2e/test_all_modes.py -vv --tb=short
"""

import subprocess
import sys
from pathlib import Path
from typing import List, Tuple

# Add simple test runner if pytest not available
try:
    import pytest
    HAVE_PYTEST = True
except ImportError:
    HAVE_PYTEST = False
    print("Note: pytest not found, using simple test runner")
    print("Install pytest for better output: python3 -m pip install pytest")


# Test configuration
BINARIES = [
    ("./bootstrap/bin/hybrid-bootstrap-main", "sync"),
    ("./bootstrap/bin/hybrid-bootstrap-main_concurrent", "async"),
]

TIMEOUT = 5  # seconds - catch hanging programs


class TestCLIContract:
    """
    Tests the CLI contract that both implementations must honor.
    Each test runs against BOTH sync and async binaries to prove
    behavioral equivalence.
    """

    if HAVE_PYTEST:
        @pytest.mark.parametrize("binary,mode", BINARIES)
        def test_help_displays_usage(self, binary: str, mode: str):
            """--help flag shows usage information and exits cleanly (exit 0)"""
            result = subprocess.run(
                [binary, "--help"],
                capture_output=True,
                text=True,
                timeout=TIMEOUT
            )

            assert result.returncode == 0, \
                f"[{mode}] --help should exit with code 0, got {result.returncode}"

            assert "Usage:" in result.stdout, \
                f"[{mode}] --help should display usage information"

            assert "Hybrid Architecture Template" in result.stdout, \
                f"[{mode}] --help should display program name"

            # Verify signal handlers are working (integration test)
            assert "Signal handlers installed" in result.stdout, \
                f"[{mode}] Signal handlers should be installed"

            assert "Signal handlers uninstalled" in result.stdout, \
                f"[{mode}] Signal handlers should be cleanly uninstalled"

        @pytest.mark.parametrize("binary,mode", BINARIES)
        def test_version_shows_version(self, binary: str, mode: str):
            """--version flag shows version number and exits cleanly (exit 0)"""
            result = subprocess.run(
                [binary, "--version"],
                capture_output=True,
                text=True,
                timeout=TIMEOUT
            )

            assert result.returncode == 0, \
                f"[{mode}] --version should exit with code 0, got {result.returncode}"

            assert "v1.0.0" in result.stdout or "1.0.0" in result.stdout, \
                f"[{mode}] --version should display version number"

        @pytest.mark.parametrize("binary,mode", BINARIES)
        @pytest.mark.parametrize("name,expected_greeting", [
            ("Ada", "Hello, Ada!"),
            ("World", "Hello, World!"),
            ("Bob", "Hello, Bob!"),
            ("Alice", "Hello, Alice!"),
        ])
        def test_valid_name_produces_greeting(
            self, binary: str, mode: str, name: str, expected_greeting: str
        ):
            """Valid names produce correct greetings and exit successfully (exit 0)"""
            result = subprocess.run(
                [binary, name],
                capture_output=True,
                text=True,
                timeout=TIMEOUT
            )

            assert result.returncode == 0, \
                f"[{mode}] Greeting '{name}' should exit with code 0, got {result.returncode}"

            assert expected_greeting in result.stdout, \
                f"[{mode}] Expected greeting '{expected_greeting}' not found in output"

        @pytest.mark.parametrize("binary,mode", BINARIES)
        def test_missing_argument_error(self, binary: str, mode: str):
            """Missing required argument produces usage error (exit 64)"""
            result = subprocess.run(
                [binary],
                capture_output=True,
                text=True,
                timeout=TIMEOUT
            )

            assert result.returncode == 64, \
                f"[{mode}] Missing argument should exit with code 64 (usage error), got {result.returncode}"

            assert "Missing required argument" in result.stdout, \
                f"[{mode}] Should display error message about missing argument"

        @pytest.mark.parametrize("binary,mode", BINARIES)
        def test_empty_name_error(self, binary: str, mode: str):
            """Empty name argument produces usage error (exit 64)"""
            result = subprocess.run(
                [binary, ""],
                capture_output=True,
                text=True,
                timeout=TIMEOUT
            )

            assert result.returncode == 64, \
                f"[{mode}] Empty name should exit with code 64 (usage error), got {result.returncode}"

        @pytest.mark.parametrize("binary,mode", BINARIES)
        def test_program_completes_without_hanging(self, binary: str, mode: str):
            """Program completes within timeout period (regression test for task hanging)"""
            # This test is implicit in all other tests due to timeout,
            # but we make it explicit here to document the requirement
            try:
                result = subprocess.run(
                    [binary, "--help"],
                    capture_output=True,
                    text=True,
                    timeout=TIMEOUT
                )
                # If we got here without TimeoutExpired, test passes
                assert True, f"[{mode}] Program completed without hanging"
            except subprocess.TimeoutExpired:
                pytest.fail(f"[{mode}] Program hung and did not complete within {TIMEOUT} seconds")


# Simple test runner for when pytest is not available
def simple_test_runner():
    """Minimal test runner when pytest is not installed"""
    print("=" * 70)
    print("E2E Tests - Hybrid Architecture Template")
    print("=" * 70)
    print()

    tests_run = 0
    tests_passed = 0
    tests_failed = 0

    test_cases = TestCLIContract()

    for binary, mode in BINARIES:
        print(f"\n[{mode.upper()}] Testing {binary}")
        print("-" * 70)

        # Test 1: Help
        tests_run += 1
        try:
            test_cases.test_help_displays_usage(binary, mode)
            print(f"✓ [{mode}] Help displays usage")
            tests_passed += 1
        except AssertionError as e:
            print(f"✗ [{mode}] Help test failed: {e}")
            tests_failed += 1

        # Test 2: Version
        tests_run += 1
        try:
            test_cases.test_version_shows_version(binary, mode)
            print(f"✓ [{mode}] Version shows version")
            tests_passed += 1
        except AssertionError as e:
            print(f"✗ [{mode}] Version test failed: {e}")
            tests_failed += 1

        # Test 3: Valid greetings
        for name, expected in [("Ada", "Hello, Ada!"), ("World", "Hello, World!")]:
            tests_run += 1
            try:
                test_cases.test_valid_name_produces_greeting(binary, mode, name, expected)
                print(f"✓ [{mode}] Greeting '{name}' produces '{expected}'")
                tests_passed += 1
            except AssertionError as e:
                print(f"✗ [{mode}] Greeting test failed: {e}")
                tests_failed += 1

        # Test 4: Error handling
        tests_run += 1
        try:
            test_cases.test_missing_argument_error(binary, mode)
            print(f"✓ [{mode}] Missing argument error handling")
            tests_passed += 1
        except AssertionError as e:
            print(f"✗ [{mode}] Error handling test failed: {e}")
            tests_failed += 1

    print()
    print("=" * 70)
    print("Test Summary")
    print("=" * 70)
    print(f"Total tests run: {tests_run}")
    print(f"Passed: {tests_passed}")
    print(f"Failed: {tests_failed}")

    if tests_failed > 0:
        print("\n❌ Some tests failed")
        sys.exit(1)
    else:
        print("\n✅ All tests passed!")
        sys.exit(0)


if __name__ == "__main__":
    if not HAVE_PYTEST:
        simple_test_runner()
    else:
        # Run with pytest
        sys.exit(pytest.main([__file__, "-v"]))
