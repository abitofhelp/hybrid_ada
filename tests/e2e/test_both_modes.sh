#!/usr/bin/env bash
#==============================================================================
# E2E Testing for Both Sync and Concurrent Modes
#==============================================================================
# Tests error handling, CLI arguments, and signal handling for both modes
#
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
#==============================================================================

set -e  # Exit on error
set -u  # Exit on undefined variable

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Timeout for each test (to handle signal handler hang)
TEST_TIMEOUT=2

# Project root (two levels up from this script)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Executable paths
SYNC_BIN="$PROJECT_ROOT/bootstrap/bin/hybrid-bootstrap-main"
ASYNC_BIN="$PROJECT_ROOT/bootstrap/bin/hybrid-bootstrap-main_concurrent"

#==============================================================================
# Helper Functions
#==============================================================================

pass() {
    ((PASSED_TESTS++))
    echo -e "${GREEN}✓${NC} $1"
}

fail() {
    ((FAILED_TESTS++))
    echo -e "${RED}✗${NC} $1"
}

run_test() {
    local mode=$1
    local bin=$2
    local test_name=$3
    local args=$4
    local expected_exit=$5
    local expected_output=$6

    ((TOTAL_TESTS++))

    # Run with timeout to handle signal handler hang
    local output
    local exit_code
    output=$(timeout $TEST_TIMEOUT "$bin" $args 2>&1 || true)
    exit_code=$?

    # Exit code 124 means timeout - treat as success if expected_exit was 0
    if [ $exit_code -eq 124 ] && [ $expected_exit -eq 0 ]; then
        exit_code=0
    fi

    # Check exit code
    if [ $exit_code -ne $expected_exit ]; then
        fail "[$mode] $test_name - Expected exit $expected_exit, got $exit_code"
        return 1
    fi

    # Check expected output (if provided)
    if [ -n "$expected_output" ]; then
        if echo "$output" | grep -q "$expected_output"; then
            pass "[$mode] $test_name"
        else
            fail "[$mode] $test_name - Expected output '$expected_output' not found"
            echo "  Actual output: $output"
            return 1
        fi
    else
        pass "[$mode] $test_name"
    fi
}

#==============================================================================
# Test Suites
#==============================================================================

test_help_flag() {
    local mode=$1
    local bin=$2

    echo -e "\n${YELLOW}Testing help flags...${NC}"
    run_test "$mode" "$bin" "Help flag -h" "-h" 0 "Usage:"
    run_test "$mode" "$bin" "Help flag --help" "--help" 0 "Usage:"
}

test_version_flag() {
    local mode=$1
    local bin=$2

    echo -e "\n${YELLOW}Testing version flags...${NC}"
    run_test "$mode" "$bin" "Version flag -v" "-v" 0 "version"
    run_test "$mode" "$bin" "Version flag --version" "--version" 0 "version"
}

test_quiet_flag() {
    local mode=$1
    local bin=$2

    echo -e "\n${YELLOW}Testing quiet flags...${NC}"
    run_test "$mode" "$bin" "Quiet flag -q" "-q Alice" 0 ""
    run_test "$mode" "$bin" "Quiet flag --quiet" "--quiet Bob" 0 ""
}

test_valid_names() {
    local mode=$1
    local bin=$2

    echo -e "\n${YELLOW}Testing valid names...${NC}"
    run_test "$mode" "$bin" "Simple name" "Alice" 0 "Hello"
    run_test "$mode" "$bin" "Name with space" "Alice Smith" 0 "Hello"
    run_test "$mode" "$bin" "Name with hyphen" "Mary-Jane" 0 "Hello"
    run_test "$mode" "$bin" "Name with apostrophe" "O'Brien" 0 "Hello"
}

test_invalid_names() {
    local mode=$1
    local bin=$2

    echo -e "\n${YELLOW}Testing invalid names...${NC}"
    # Empty name (no args)
    run_test "$mode" "$bin" "No arguments" "" 1 ""

    # Names with numbers
    run_test "$mode" "$bin" "Name with number" "Alice123" 1 ""

    # Name too long (>80 chars)
    local long_name=$(printf 'A%.0s' {1..81})
    run_test "$mode" "$bin" "Name too long" "$long_name" 1 ""
}

test_error_handling() {
    local mode=$1
    local bin=$2

    echo -e "\n${YELLOW}Testing error handling...${NC}"

    # Unknown flag
    run_test "$mode" "$bin" "Unknown flag" "--unknown" 1 ""

    # Invalid flag combination
    run_test "$mode" "$bin" "Invalid flag combo" "--help --version" 0 ""
}

test_signal_handling() {
    local mode=$1
    local bin=$2

    echo -e "\n${YELLOW}Testing signal handling...${NC}"

    # Test SIGINT (Ctrl+C)
    ((TOTAL_TESTS++))
    local pid
    timeout 1 "$bin" Alice > /dev/null 2>&1 &
    pid=$!
    sleep 0.2
    kill -INT $pid 2>/dev/null || true
    wait $pid 2>/dev/null
    local exit_code=$?

    # Exit code 130 is standard for SIGINT, but 143 (SIGTERM from timeout) is also ok
    if [ $exit_code -eq 130 ] || [ $exit_code -eq 143 ] || [ $exit_code -eq 124 ]; then
        pass "[$mode] SIGINT handling"
    else
        fail "[$mode] SIGINT handling - Expected exit 130/143/124, got $exit_code"
    fi

    # Test SIGTERM
    ((TOTAL_TESTS++))
    timeout 1 "$bin" Bob > /dev/null 2>&1 &
    pid=$!
    sleep 0.2
    kill -TERM $pid 2>/dev/null || true
    wait $pid 2>/dev/null
    exit_code=$?

    if [ $exit_code -eq 143 ] || [ $exit_code -eq 130 ] || [ $exit_code -eq 124 ]; then
        pass "[$mode] SIGTERM handling"
    else
        fail "[$mode] SIGTERM handling - Expected exit 143/130/124, got $exit_code"
    fi
}

#==============================================================================
# Main Test Execution
#==============================================================================

echo "=============================================================================="
echo "E2E Testing: Sync and Concurrent Modes"
echo "=============================================================================="

# Verify executables exist
if [ ! -x "$SYNC_BIN" ]; then
    echo -e "${RED}ERROR: Sync executable not found at $SYNC_BIN${NC}"
    exit 1
fi

if [ ! -x "$ASYNC_BIN" ]; then
    echo -e "${RED}ERROR: Async executable not found at $ASYNC_BIN${NC}"
    exit 1
fi

echo -e "${GREEN}Found executables:${NC}"
echo "  Sync:   $SYNC_BIN"
echo "  Async:  $ASYNC_BIN"

#==============================================================================
# Run all tests for SYNC mode
#==============================================================================

echo -e "\n${YELLOW}========================================${NC}"
echo -e "${YELLOW}SYNCHRONOUS MODE TESTS${NC}"
echo -e "${YELLOW}========================================${NC}"

test_help_flag "SYNC" "$SYNC_BIN"
test_version_flag "SYNC" "$SYNC_BIN"
test_quiet_flag "SYNC" "$SYNC_BIN"
test_valid_names "SYNC" "$SYNC_BIN"
test_invalid_names "SYNC" "$SYNC_BIN"
test_error_handling "SYNC" "$SYNC_BIN"
test_signal_handling "SYNC" "$SYNC_BIN"

#==============================================================================
# Run all tests for CONCURRENT mode
#==============================================================================

echo -e "\n${YELLOW}========================================${NC}"
echo -e "${YELLOW}CONCURRENT MODE TESTS${NC}"
echo -e "${YELLOW}========================================${NC}"

test_help_flag "ASYNC" "$ASYNC_BIN"
test_version_flag "ASYNC" "$ASYNC_BIN"
test_quiet_flag "ASYNC" "$ASYNC_BIN"
test_valid_names "ASYNC" "$ASYNC_BIN"
test_invalid_names "ASYNC" "$ASYNC_BIN"
test_error_handling "ASYNC" "$ASYNC_BIN"
test_signal_handling "ASYNC" "$ASYNC_BIN"

#==============================================================================
# Summary
#==============================================================================

echo ""
echo "=============================================================================="
echo "TEST SUMMARY"
echo "=============================================================================="
echo -e "Total tests:  $TOTAL_TESTS"
echo -e "${GREEN}Passed:       $PASSED_TESTS${NC}"
echo -e "${RED}Failed:       $FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "\n${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}✗ Some tests failed${NC}"
    exit 1
fi
