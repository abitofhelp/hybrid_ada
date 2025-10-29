# Testing Guide

**Version:** 1.0.0  
**Date:** October 28, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  


## Overview

This project uses AUnit as the primary testing framework, with support for unit, integration, and end-to-end tests. The test suite is organized in a separate Alire crate to keep test dependencies isolated from the main project.

## Testing Stack

- **Unit Testing**: AUnit 24.0.0+
- **Integration Testing**: AUnit + scripted_testing
- **E2E Testing**: Custom process execution tests
- **Coverage**: GNATcov / lcov
- **CI/CD**: GitHub Actions

## Project Structure

```
tests/
├── alire.toml                  # Test crate configuration
├── hybrid_tests.gpr            # Test project file
├── src/
│   ├── test_runner.adb         # Main test executable
│   ├── test_suite.ads/adb      # Root test suite
│   ├── unit/                   # Unit tests
│   │   ├── domain/             # Domain layer tests
│   │   ├── application/        # Application layer tests
│   │   ├── infrastructure/     # Infrastructure tests
│   │   └── presentation/       # Presentation tests
│   ├── integration/            # Integration tests
│   └── e2e/                    # End-to-end tests
```

## Running Tests

### Basic Test Execution

```bash
# Run all tests
make test

# Or directly
./run_tests.sh

# Run tests in release mode
./run_tests.sh release
```

### Watch Mode

Automatically run tests when files change:

```bash
make watch
# Or
./test_watch.sh
```

### Coverage Analysis

```bash
# Run tests with coverage
make coverage

# Coverage report will be in tests/coverage_html/index.html
```

## Writing Tests

### Unit Test Example

```ada
with AUnit.Test_Cases;
with AUnit.Assertions;

package body My_Tests is
   use AUnit.Assertions;

   procedure Test_Something (T : in out Test_Case'Class) is
   begin
      -- Arrange
      declare
         Input : constant String := "test";
         Expected : constant String := "TEST";
      begin
         -- Act
         Result := To_Upper (Input);
         
         -- Assert
         Assert (Result = Expected, 
                 "Expected '" & Expected & "' but got '" & Result & "'");
      end;
   end Test_Something;
end My_Tests;
```

### Test Organization

1. **Unit Tests**: Test individual components in isolation
   - One test file per component
   - Mock dependencies
   - Fast execution

2. **Integration Tests**: Test component interactions
   - Test multiple components together
   - Use real implementations where possible
   - May use external resources

3. **E2E Tests**: Test complete user scenarios
   - Test the CLI application
   - Verify signal handling
   - Test error conditions

## Mocking and Test Doubles

Example mock implementation:

```ada
type Mock_Output_Port is new Output.Output_Port with record
   Last_Message : Unbounded_String;
   Call_Count   : Natural := 0;
   Should_Fail  : Boolean := False;
end record;

overriding procedure Send
  (Self    : Mock_Output_Port;
   Message : String;
   Result  : out Output.Result) is
begin
   Self.Call_Count := Self.Call_Count + 1;
   Self.Last_Message := To_Unbounded_String (Message);
   
   if Self.Should_Fail then
      Result := Output.Output_Result.Error (...);
   else
      Result := Output.Output_Result.Success (True);
   end if;
end Send;
```

## Continuous Integration

Tests run automatically on:
- Push to main/develop branches
- Pull requests
- Tagged releases

### CI Matrix

- **Operating Systems**: Ubuntu, macOS, Windows
- **Build Modes**: Debug, Release, Coverage
- **Checks**: Tests, Style, Static Analysis

## Best Practices

1. **Test Naming**: Use descriptive names that explain what is being tested
2. **Test Independence**: Each test should be independent and not rely on others
3. **Fast Tests**: Keep unit tests fast (< 1ms per test)
4. **Clear Assertions**: Use meaningful assertion messages
5. **Test Coverage**: Aim for >80% coverage on critical paths
6. **Error Cases**: Always test error conditions and edge cases

## Troubleshooting

### Tests Won't Compile

```bash
cd tests
alr update
alr build
```

### Missing Dependencies

Ensure all test dependencies are in `tests/alire.toml`:
```toml
[[depends-on]]
aunit = "^24.0.0"
gnatcov = "*"
scripted_testing = "*"
```

### Coverage Not Working

Install coverage tools:
```bash
# Ubuntu/Debian
sudo apt-get install lcov

# macOS
brew install lcov
```

## Advanced Topics

### Parameterized Tests

Create data-driven tests:

```ada
type Test_Data is record
   Input    : String (1 .. 20);
   In_Len   : Natural;
   Expected : String (1 .. 20);
   Exp_Len  : Natural;
end record;

Test_Cases : constant array (1 .. 3) of Test_Data := ...;

for TC of Test_Cases loop
   -- Run test with TC.Input and verify TC.Expected
end loop;
```

### Performance Tests

Measure execution time:

```ada
Start_Time := Ada.Real_Time.Clock;
-- Run operation
End_Time := Ada.Real_Time.Clock;
Duration := To_Duration (End_Time - Start_Time);
Assert (Duration < 0.001, "Operation too slow: " & Duration'Image);
```

### Concurrent Tests

Test concurrent behavior:

```ada
task type Test_Worker is
   entry Start;
   entry Get_Result (Success : out Boolean);
end Test_Worker;

Workers : array (1 .. 10) of Test_Worker;
-- Start all workers and verify thread safety
```