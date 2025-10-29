#!/usr/bin/env python3
"""
E2E CLI Tests - Test all command-line flags and options
=========================================================
Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
SPDX-License-Identifier: BSD-3-Clause
"""

import subprocess
import sys
from pathlib import Path
from typing import Tuple, List

# Colors for terminal output
GREEN = '\033[0;32m'
RED = '\033[0;31m'
YELLOW = '\033[0;33m'
NC = '\033[0m'  # No Color

# Test counters
tests_run = 0
tests_passed = 0
tests_failed = 0
failed_tests: List[str] = []

# Binary path
BINARY = Path("./bootstrap/bin/hybrid-bootstrap-main")
TIMEOUT = 2  # seconds


def test_passed(name: str) -> None:
    """Mark a test as passed."""
    global tests_run, tests_passed
    tests_passed += 1
    tests_run += 1
    print(f"{GREEN}✓{NC} {name}")


def test_failed(name: str, expected: str = "", got: str = "") -> None:
    """Mark a test as failed."""
    global tests_run, tests_failed, failed_tests
    tests_failed += 1
    tests_run += 1
    failed_tests.append(name)
    print(f"{RED}✗{NC} {name}")
    if expected:
        print(f"  {YELLOW}Expected:{NC} {expected}")
    if got:
        print(f"  {YELLOW}Got:{NC} {got}")


def run_command(args: List[str], timeout: int = TIMEOUT) -> Tuple[int, str]:
    """
    Run a command with timeout and return (exit_code, combined_output).
    Handles the signal handler task cleanup issue by using timeout.
    """
    try:
        result = subprocess.run(
            [str(BINARY)] + args,
            capture_output=True,
            text=True,
            timeout=timeout
        )
        output = result.stdout + result.stderr
        return (result.returncode, output)
    except subprocess.TimeoutExpired:
        # Binary didn't exit in time - this is expected due to signal handler issue
        # Kill the process and return timeout error
        return (124, "")  # 124 is timeout exit code
    except Exception as e:
        return (1, str(e))


def test_exit_code(name: str, expected: int, actual: int) -> None:
    """Test if exit code matches expected."""
    if actual == expected:
        test_passed(name)
    else:
        test_failed(name, f"exit code {expected}", f"exit code {actual}")


def test_output_contains(name: str, output: str, expected: str) -> None:
    """Test if output contains expected string."""
    if expected in output:
        test_passed(name)
    else:
        test_failed(name, f"output containing '{expected}'", f"output: {output[:100]}")


def test_output_not_contains(name: str, output: str, unexpected: str) -> None:
    """Test if output does NOT contain unexpected string."""
    if unexpected not in output:
        test_passed(name)
    else:
        test_failed(name, f"output NOT containing '{unexpected}'", f"output: {output[:100]}")


def main() -> int:
    """Run all E2E tests."""
    print("E2E CLI Tests for Hybrid Architecture")
    print("=" * 42)
    print()

    # Check binary exists
    if not BINARY.exists():
        print(f"{RED}ERROR: Binary not found at {BINARY}{NC}")
        print("Run 'make build' first")
        return 1

    print(f"{GREEN}✓{NC} Binary found: {BINARY}")
    print()

    # ==========================================================================
    # Test 1: Help Flag (-h)
    # ==========================================================================
    print("Test Group: Help Flag")
    print("-" * 22)

    exit_code, output = run_command(["-h"])
    test_exit_code("Help flag (-h) exits with 0", 0, exit_code)
    test_output_contains("Help output contains 'Usage'", output, "Usage:")
    test_output_contains("Help output contains 'Options'", output, "Options:")
    test_output_contains("Help output contains '-h, --help'", output, "-h, --help")
    test_output_contains("Help output contains '-v, --version'", output, "-v, --version")
    test_output_contains("Help output contains '-q, --quiet'", output, "-q, --quiet")
    print()

    # ==========================================================================
    # Test 2: Help Flag (--help)
    # ==========================================================================
    print("Test Group: Help Flag (long form)")
    print("-" * 34)

    exit_code, output = run_command(["--help"])
    test_exit_code("Help flag (--help) exits with 0", 0, exit_code)
    test_output_contains("Help long form contains 'Usage'", output, "Usage:")
    print()

    # ==========================================================================
    # Test 3: Version Flag (-v)
    # ==========================================================================
    print("Test Group: Version Flag")
    print("-" * 24)

    exit_code, output = run_command(["-v"])
    test_exit_code("Version flag (-v) exits with 0", 0, exit_code)
    test_output_contains("Version output contains version number", output, "v1.0.0")
    test_output_contains("Version output contains 'Hybrid'", output, "Hybrid")
    print()

    # ==========================================================================
    # Test 4: Version Flag (--version)
    # ==========================================================================
    print("Test Group: Version Flag (long form)")
    print("-" * 37)

    exit_code, output = run_command(["--version"])
    test_exit_code("Version flag (--version) exits with 0", 0, exit_code)
    test_output_contains("Version long form contains version", output, "v1.0.0")
    print()

    # ==========================================================================
    # Test 5: Valid Name Argument
    # ==========================================================================
    print("Test Group: Valid Name Argument")
    print("-" * 32)

    exit_code, output = run_command(["Alice"])
    test_exit_code("Valid name 'Alice' exits with 0", 0, exit_code)
    test_output_contains("Greeting contains 'Hello'", output, "Hello")
    test_output_contains("Greeting contains name 'Alice'", output, "Alice")
    print()

    # ==========================================================================
    # Test 6: Valid Name with Spaces
    # ==========================================================================
    print("Test Group: Names with Spaces")
    print("-" * 30)

    exit_code, output = run_command(["Alice Smith"])
    test_exit_code("Name with space 'Alice Smith' exits with 0", 0, exit_code)
    test_output_contains("Greeting contains 'Alice Smith'", output, "Alice Smith")
    print()

    # ==========================================================================
    # Test 7: Valid Name with Hyphen
    # ==========================================================================
    print("Test Group: Names with Hyphens")
    print("-" * 31)

    exit_code, output = run_command(["Mary-Jane"])
    test_exit_code("Name with hyphen 'Mary-Jane' exits with 0", 0, exit_code)
    test_output_contains("Greeting contains 'Mary-Jane'", output, "Mary-Jane")
    print()

    # ==========================================================================
    # Test 8: Empty Name (Error Case)
    # ==========================================================================
    print("Test Group: Invalid Input - Empty Name")
    print("-" * 39)

    exit_code, output = run_command([""])
    test_exit_code("Empty name exits with error code 1", 1, exit_code)
    test_output_contains("Error message shown for empty name", output, "Error:")
    print()

    # ==========================================================================
    # Test 9: Name Too Long (Error Case)
    # ==========================================================================
    print("Test Group: Invalid Input - Name Too Long")
    print("-" * 42)

    long_name = "A" * 100
    exit_code, output = run_command([long_name])
    test_exit_code("Name too long exits with error code 1", 1, exit_code)
    test_output_contains("Error message shown for long name", output, "Error:")
    print()

    # ==========================================================================
    # Test 10: Name with Invalid Characters (Error Case)
    # ==========================================================================
    print("Test Group: Invalid Input - Invalid Characters")
    print("-" * 47)

    exit_code, output = run_command(["Alice123"])
    test_exit_code("Name with numbers exits with error code 1", 1, exit_code)
    test_output_contains("Error message shown for invalid chars", output, "Error:")
    print()

    # ==========================================================================
    # Test 11: Quiet Flag (-q)
    # ==========================================================================
    print("Test Group: Quiet Flag")
    print("-" * 22)

    exit_code, output = run_command(["-q", "Alice"])
    test_exit_code("Quiet flag (-q) exits with 0", 0, exit_code)
    test_output_not_contains("Quiet mode suppresses greeting", output, "Hello")
    print()

    # ==========================================================================
    # Test 12: Quiet Flag (--quiet)
    # ==========================================================================
    print("Test Group: Quiet Flag (long form)")
    print("-" * 35)

    exit_code, output = run_command(["--quiet", "Bob"])
    test_exit_code("Quiet flag (--quiet) exits with 0", 0, exit_code)
    test_output_not_contains("Quiet long form suppresses greeting", output, "Hello")
    print()

    # ==========================================================================
    # Test 13: No Arguments (Error Case)
    # ==========================================================================
    print("Test Group: Missing Arguments")
    print("-" * 30)

    exit_code, output = run_command([])
    test_exit_code("No arguments exits with error code 1", 1, exit_code)
    test_output_contains("Shows usage when no args", output, "Usage:")
    print()

    # ==========================================================================
    # Test 14: Unknown Flag (Error Case)
    # ==========================================================================
    print("Test Group: Unknown Flags")
    print("-" * 25)

    exit_code, output = run_command(["--unknown", "Alice"])
    test_exit_code("Unknown flag exits with error code 1", 1, exit_code)
    test_output_contains("Shows error for unknown flag", output, "Error:")
    print()

    # ==========================================================================
    # Test 15: Multiple Flags Combined
    # ==========================================================================
    print("Test Group: Flag Combinations")
    print("-" * 30)

    exit_code, output = run_command(["--help", "--quiet", "Alice"])
    test_exit_code("Help flag takes precedence", 0, exit_code)
    test_output_contains("Shows help when combined with other flags", output, "Usage:")
    print()

    # ==========================================================================
    # Test Summary
    # ==========================================================================
    print()
    print("=" * 46)
    print("Test Summary")
    print("=" * 46)
    print(f"Tests run:    {tests_run}")
    print(f"Tests passed: {GREEN}{tests_passed}{NC}")
    print(f"Tests failed: {RED}{tests_failed}{NC}")
    print()

    if failed_tests:
        print("Failed tests:")
        for test in failed_tests:
            print(f"  {RED}✗{NC} {test}")
        print()
        return 1
    else:
        print(f"{GREEN}All tests passed!{NC}")
        return 0


if __name__ == "__main__":
    sys.exit(main())
