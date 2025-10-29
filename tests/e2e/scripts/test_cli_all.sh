#!/bin/bash
# =============================================================================
# E2E CLI Tests - Test all command-line flags and options
# =============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# =============================================================================

set -e  # Exit on error

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Binary path
BINARY="./bootstrap/bin/hybrid-bootstrap-main"

# Test result tracking
FAILED_TESTS=()

# =============================================================================
# Helper Functions
# =============================================================================

test_passed() {
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
    echo -e "${GREEN}✓${NC} $1"
}

test_failed() {
    ((TESTS_FAILED++))
    ((TESTS_RUN++))
    FAILED_TESTS+=("$1")
    echo -e "${RED}✗${NC} $1"
    if [ -n "$2" ]; then
        echo -e "  ${YELLOW}Expected:${NC} $2"
    fi
    if [ -n "$3" ]; then
        echo -e "  ${YELLOW}Got:${NC} $3"
    fi
}

test_exit_code() {
    local test_name="$1"
    local expected_code=$2
    local actual_code=$3

    if [ "$actual_code" -eq "$expected_code" ]; then
        test_passed "$test_name"
    else
        test_failed "$test_name" "exit code $expected_code" "exit code $actual_code"
    fi
}

test_output_contains() {
    local test_name="$1"
    local output="$2"
    local expected="$3"

    if echo "$output" | grep -q "$expected"; then
        test_passed "$test_name"
    else
        test_failed "$test_name" "output containing '$expected'" "output: $output"
    fi
}

test_output_not_contains() {
    local test_name="$1"
    local output="$2"
    local unexpected="$3"

    if ! echo "$output" | grep -q "$unexpected"; then
        test_passed "$test_name"
    else
        test_failed "$test_name" "output NOT containing '$unexpected'" "output: $output"
    fi
}

# =============================================================================
# Prerequisite Checks
# =============================================================================

echo "E2E CLI Tests for Hybrid Architecture"
echo "======================================"
echo

if [ ! -f "$BINARY" ]; then
    echo -e "${RED}ERROR: Binary not found at $BINARY${NC}"
    echo "Run 'make build' first"
    exit 1
fi

echo -e "${GREEN}✓${NC} Binary found: $BINARY"
echo

# =============================================================================
# Test 1: Help Flag (-h)
# =============================================================================

echo "Test Group: Help Flag"
echo "----------------------"

output=$($BINARY -h 2>&1)
exit_code=$?

test_exit_code "Help flag (-h) exits with 0" 0 $exit_code
test_output_contains "Help output contains 'Usage'" "$output" "Usage:"
test_output_contains "Help output contains 'Options'" "$output" "Options:"
test_output_contains "Help output contains '-h, --help'" "$output" "-h, --help"
test_output_contains "Help output contains '-v, --version'" "$output" "-v, --version"
test_output_contains "Help output contains '-q, --quiet'" "$output" "-q, --quiet"

echo

# =============================================================================
# Test 2: Help Flag (--help)
# =============================================================================

echo "Test Group: Help Flag (long form)"
echo "----------------------------------"

output=$($BINARY --help 2>&1)
exit_code=$?

test_exit_code "Help flag (--help) exits with 0" 0 $exit_code
test_output_contains "Help long form contains 'Usage'" "$output" "Usage:"

echo

# =============================================================================
# Test 3: Version Flag (-v)
# =============================================================================

echo "Test Group: Version Flag"
echo "------------------------"

output=$($BINARY -v 2>&1)
exit_code=$?

test_exit_code "Version flag (-v) exits with 0" 0 $exit_code
test_output_contains "Version output contains version number" "$output" "v1.0.0"
test_output_contains "Version output contains 'Hybrid'" "$output" "Hybrid"

echo

# =============================================================================
# Test 4: Version Flag (--version)
# =============================================================================

echo "Test Group: Version Flag (long form)"
echo "-------------------------------------"

output=$($BINARY --version 2>&1)
exit_code=$?

test_exit_code "Version flag (--version) exits with 0" 0 $exit_code
test_output_contains "Version long form contains version" "$output" "v1.0.0"

echo

# =============================================================================
# Test 5: Valid Name Argument
# =============================================================================

echo "Test Group: Valid Name Argument"
echo "--------------------------------"

output=$($BINARY Alice 2>&1)
exit_code=$?

test_exit_code "Valid name 'Alice' exits with 0" 0 $exit_code
test_output_contains "Greeting contains 'Hello'" "$output" "Hello"
test_output_contains "Greeting contains name 'Alice'" "$output" "Alice"

echo

# =============================================================================
# Test 6: Valid Name with Spaces
# =============================================================================

echo "Test Group: Names with Spaces"
echo "------------------------------"

output=$($BINARY "Alice Smith" 2>&1)
exit_code=$?

test_exit_code "Name with space 'Alice Smith' exits with 0" 0 $exit_code
test_output_contains "Greeting contains 'Alice Smith'" "$output" "Alice Smith"

echo

# =============================================================================
# Test 7: Valid Name with Hyphen
# =============================================================================

echo "Test Group: Names with Hyphens"
echo "-------------------------------"

output=$($BINARY "Mary-Jane" 2>&1)
exit_code=$?

test_exit_code "Name with hyphen 'Mary-Jane' exits with 0" 0 $exit_code
test_output_contains "Greeting contains 'Mary-Jane'" "$output" "Mary-Jane"

echo

# =============================================================================
# Test 8: Empty Name (Error Case)
# =============================================================================

echo "Test Group: Invalid Input - Empty Name"
echo "---------------------------------------"

output=$($BINARY "" 2>&1) || exit_code=$?

test_exit_code "Empty name exits with error code 1" 1 $exit_code
test_output_contains "Error message shown for empty name" "$output" "Error:"

echo

# =============================================================================
# Test 9: Name Too Long (Error Case)
# =============================================================================

echo "Test Group: Invalid Input - Name Too Long"
echo "------------------------------------------"

# Generate a name longer than 80 characters
long_name=$(printf 'A%.0s' {1..100})
output=$($BINARY "$long_name" 2>&1) || exit_code=$?

test_exit_code "Name too long exits with error code 1" 1 $exit_code
test_output_contains "Error message shown for long name" "$output" "Error:"

echo

# =============================================================================
# Test 10: Name with Invalid Characters (Error Case)
# =============================================================================

echo "Test Group: Invalid Input - Invalid Characters"
echo "-----------------------------------------------"

output=$($BINARY "Alice123" 2>&1) || exit_code=$?

test_exit_code "Name with numbers exits with error code 1" 1 $exit_code
test_output_contains "Error message shown for invalid chars" "$output" "Error:"

echo

# =============================================================================
# Test 11: Quiet Flag (-q)
# =============================================================================

echo "Test Group: Quiet Flag"
echo "----------------------"

output=$($BINARY -q Alice 2>&1)
exit_code=$?

test_exit_code "Quiet flag (-q) exits with 0" 0 $exit_code
test_output_not_contains "Quiet mode suppresses greeting" "$output" "Hello"

echo

# =============================================================================
# Test 12: Quiet Flag (--quiet)
# =============================================================================

echo "Test Group: Quiet Flag (long form)"
echo "-----------------------------------"

output=$($BINARY --quiet Bob 2>&1)
exit_code=$?

test_exit_code "Quiet flag (--quiet) exits with 0" 0 $exit_code
test_output_not_contains "Quiet long form suppresses greeting" "$output" "Hello"

echo

# =============================================================================
# Test 13: No Arguments (Error Case)
# =============================================================================

echo "Test Group: Missing Arguments"
echo "------------------------------"

output=$($BINARY 2>&1) || exit_code=$?

test_exit_code "No arguments exits with error code 1" 1 $exit_code
test_output_contains "Shows usage when no args" "$output" "Usage:"

echo

# =============================================================================
# Test 14: Unknown Flag (Error Case)
# =============================================================================

echo "Test Group: Unknown Flags"
echo "-------------------------"

output=$($BINARY --unknown Alice 2>&1) || exit_code=$?

test_exit_code "Unknown flag exits with error code 1" 1 $exit_code
test_output_contains "Shows error for unknown flag" "$output" "Error:"

echo

# =============================================================================
# Test 15: Multiple Flags Combined
# =============================================================================

echo "Test Group: Flag Combinations"
echo "------------------------------"

# Help should take precedence
output=$($BINARY --help --quiet Alice 2>&1)
exit_code=$?

test_exit_code "Help flag takes precedence" 0 $exit_code
test_output_contains "Shows help when combined with other flags" "$output" "Usage:"

echo

# =============================================================================
# Test Summary
# =============================================================================

echo
echo "=============================================="
echo "Test Summary"
echo "=============================================="
echo "Tests run:    $TESTS_RUN"
echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
echo

if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
    echo "Failed tests:"
    for test in "${FAILED_TESTS[@]}"; do
        echo -e "  ${RED}✗${NC} $test"
    done
    echo
    exit 1
else
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi
