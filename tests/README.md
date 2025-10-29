# Hybrid Architecture - Test Suite

**Comprehensive testing framework demonstrating professional Ada 2022 testing practices**

---

## Overview

This test suite demonstrates three distinct testing approaches aligned with industry best practices:

1. **Unit Tests** - Individual components tested in complete isolation
2. **Integration Tests** - Layer boundaries tested with real implementations
3. **End-to-End Tests** - Complete application stack validation

---

## Directory Structure

```
tests/
├── src/                              # Test harness and execution
│   ├── test_runner.adb              # Main test entry point
│   ├── test_suite.ads               # Suite declaration
│   └── test_suite.adb               # Suite implementation (aggregates all tests)
│
├── unit/                            # Unit tests organized by architectural layer
│   └── src/
│       ├── domain/                  # Domain layer unit tests
│       │   ├── test_domain_value_person_name.ads/adb
│       │   ├── test_domain_model_result.ads/adb
│       │   ├── test_domain_model_either.ads/adb
│       │   ├── test_domain_error.ads/adb
│       │   └── test_domain_service_greeting.ads/adb
│       ├── application/             # Application layer unit tests
│       │   ├── test_application_error.ads/adb
│       │   └── test_application_service_create_greeting.ads/adb
│       ├── infrastructure/          # Infrastructure layer unit tests
│       │   └── test_infrastructure_adapter_console_output.ads/adb
│       └── presentation/            # Presentation layer unit tests
│           └── test_presentation_cli.ads/adb
│
├── integration/                     # Integration tests (layer boundaries)
│   └── src/
│       └── test_integration_create_greeting.ads/adb
│
├── e2e/                            # End-to-end tests (full stack)
│   └── src/
│       └── test_e2e_greeting.ads/adb
│
├── common/                          # Shared test utilities
│   └── mocks/                       # Mock implementations
│       ├── mock_output_port.ads     # Mock Output_Port for unit testing
│       └── mock_output_port.adb
│
├── obj/                             # Build artifacts
├── bin/                             # Test executables
│   └── test_runner                  # Compiled test suite
│
├── tests.gpr                        # GPR project file
└── README.md                        # This file
```

---

## Test Types Explained

### Unit Tests (`tests/unit/src/`)

**Purpose:** Test individual components in complete isolation

**Characteristics:**
- Use mocks/stubs for all dependencies
- Fast execution
- Focused on single component behavior
- Tests one layer at a time

**Organization:**
- `domain/` - Value objects, models, domain services, domain errors
- `application/` - Use cases, application errors, port interfaces
- `infrastructure/` - Adapters tested in isolation
- `presentation/` - CLI components and dependency wiring

**Example:**
```ada
-- Testing Create_Greeting use case with Mock_Output_Port
Use_Case.Execute (Name => "Alice", Quiet => False, Result => Result);
Assert (Create_Greeting.Use_Case_Result.Is_Ok (Result), "Should succeed");
```

### Integration Tests (`tests/integration/src/`)

**Purpose:** Test interaction between 2-3 adjacent layers

**Characteristics:**
- Use **real implementations** (no mocks) for tested layers
- Verify data flows correctly across layer boundaries
- Test port/adapter pattern integration
- Validate error propagation between layers

**Key Difference from Unit Tests:**
- Uses real `Console_Output_Adapter` instead of `Mock_Output_Port`
- Tests actual layer interactions, not isolated behavior

**Example:**
```ada
-- Testing Application + Infrastructure integration
Adapter : aliased Console_Output.Console_Output_Adapter;  -- Real adapter!
Use_Case.Execute (Name => "Bob", Quiet => True, Result => Result);
Assert (Create_Greeting.Use_Case_Result.Is_Ok (Result), "Integration should succeed");
```

### End-to-End Tests (`tests/e2e/src/`)

**Purpose:** Test complete application stack from top to bottom

**Characteristics:**
- Wire up all real components (no mocks)
- Test Domain → Application → Infrastructure → Presentation
- Validate complete user workflows
- Ensure entire system integrates correctly

**Key Difference from Integration Tests:**
- Tests ALL layers including Presentation and Bootstrap
- Validates complete user-facing workflows

**Example:**
```ada
-- Testing full stack including CLI application
App : constant CLI.CLI_Application := CLI.Create (Use_Case'Access, Output'Access);
Assert (App.Use_Case /= null, "Complete stack should wire correctly");
```

---

## Test Statistics

- **Total Test Suites:** 11
- **Total Test Cases:** 73+ assertions
- **Pass Rate:** 100% (0 failures)
- **Coverage:** 100% business logic
- **Framework:** AUnit 24.0.0

### By Layer

| Layer | Test Suites | Test Cases | Type |
|-------|-------------|------------|------|
| Domain | 5 | 44 | Unit |
| Application | 2 | 11 | Unit |
| Infrastructure | 1 | 5 | Unit |
| Presentation | 1 | 3 | Unit |
| Integration | 1 | 4 | Integration |
| E2E | 1 | 4 | E2E |

---

## Running Tests

### Build Tests

```bash
# Using Alire (recommended)
alr exec -- gprbuild -P tests.gpr -p

# Using GPRbuild directly
gprbuild -P tests.gpr -p
```

### Execute Test Suite

```bash
./tests/bin/test_runner

# Or via Makefile
make test-run
```

### Coverage Analysis

Generate HTML coverage reports to identify untested code:

```bash
# Run tests with coverage instrumentation
make test-coverage

# View coverage report (macOS)
open coverage/coverage.html

# View coverage report (Linux)
xdg-open coverage/coverage.html
```

**Requirements:**
- **gcovr** for HTML reports: `pip3 install gcovr`
- Without gcovr, basic `.gcov` files are generated in `coverage/`

**Coverage Files:**
- `tests-coverage.gpr` - Project file without style checks (avoids instrumentation conflicts)
- `coverage/coverage.html` - Main HTML coverage report
- `coverage/*.html` - Detailed per-file coverage reports
- `tests/obj-coverage/` - Instrumented object files (excluded from version control)

**What Gets Measured:**
- Line coverage (which lines executed)
- Branch coverage (which code paths taken)
- Function coverage (which functions called)
- Excludes test code itself (only measures production code)

### Expected Output

```
=========================================
Hybrid Architecture Ada 2022 Test Suite
=========================================

OK Domain.Value.PersonName Tests
OK Domain.Model.Result Monad Tests
OK Domain.Model.Either Monad Tests
OK Domain.Error Types Tests
OK Domain.Service.Greeting Tests
OK Application.Error Tests
OK Application.Service.Create_Greeting Tests
OK Infrastructure.Adapter.Console_Output Tests
OK Presentation.CLI Tests
OK Integration: Application + Infrastructure
OK E2E Integration Tests

Total Tests Run:   11
Successful Tests:  11
Failed Assertions: 0
Unexpected Errors: 0

Test execution completed.
```

---

## Test Framework

### AUnit

This project uses **AUnit 24.0.0** - Ada's standard unit testing framework.

**Dependencies:**
```toml
[[depends-on]]
aunit = "^24.0.0"
```

**Test Structure:**
```ada
package Test_Example is
   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;
   overriding procedure Run_Test (T : in out Test);
end Test_Example;
```

### Test Aggregation

All tests are aggregated in `test_suite.adb`:

```ada
function Suite return Access_Test_Suite is
   Ret : constant Access_Test_Suite := new Test_Suite;
begin
   --  Add all test cases
   Ret.Add_Test (Test_Case_Access'(new Test_Domain_Value_Person_Name.Test));
   Ret.Add_Test (Test_Case_Access'(new Test_Application_Error.Test));
   -- ... more tests
   return Ret;
end Suite;
```

---

## Writing New Tests

### Adding a Unit Test

1. **Create test files in appropriate layer directory:**
   ```bash
   # For domain layer test
   touch tests/unit/src/domain/test_domain_my_component.ads
   touch tests/unit/src/domain/test_domain_my_component.adb
   ```

2. **Define test specification (.ads):**
   ```ada
   pragma Ada_2022;

   with AUnit;
   with AUnit.Simple_Test_Cases;

   package Test_Domain_My_Component is
      type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

      overriding function Name (T : Test) return AUnit.Message_String;
      overriding procedure Run_Test (T : in out Test);
   end Test_Domain_My_Component;
   ```

3. **Implement tests (.adb):**
   ```ada
   with AUnit.Assertions; use AUnit.Assertions;

   package body Test_Domain_My_Component is
      overriding function Name (T : Test) return AUnit.Message_String is
         pragma Unreferenced (T);
      begin
         return AUnit.Format ("Domain.My_Component Tests");
      end Name;

      overriding procedure Run_Test (T : in out Test) is
         pragma Unreferenced (T);
      begin
         --  Test 1: Component creation
         declare
            Component : My_Component.Type := My_Component.Create;
         begin
            Assert (My_Component.Is_Valid (Component), "Should create valid component");
         end;

         --  Test 2: Component behavior
         -- ... more tests
      end Run_Test;
   end Test_Domain_My_Component;
   ```

4. **Add to test suite (`tests/src/test_suite.adb`):**
   ```ada
   with Test_Domain_My_Component;

   --  In Suite function:
   Ret.Add_Test (Test_Case_Access'(new Test_Domain_My_Component.Test));
   ```

5. **Build and run:**
   ```bash
   alr exec -- gprbuild -P tests.gpr -p
   ./tests/bin/test_runner
   ```

### Adding an Integration Test

Follow the same pattern but place files in `tests/integration/src/` and use real implementations instead of mocks.

### Adding an E2E Test

Place files in `tests/e2e/src/` and wire up the complete application stack.

---

## Mock Objects

### Available Mocks

**Mock_Output_Port** (`tests/common/mocks/mock_output_port.ads/adb`)
- Mock implementation of `Hybrid.Application.Port.Output.Output_Port`
- Always returns success for `Send` operations
- Used in unit tests to isolate use cases from infrastructure

**Usage:**
```ada
with Mock_Output_Port;

declare
   Mock : aliased Mock_Output_Port.Mock_Output_Port_Type;
   Use_Case : constant Create_Greeting.Create_Greeting_Use_Case :=
     Create_Greeting.Create (Service'Access, Mock'Access);
begin
   --  Test use case with mock
   Use_Case.Execute (Name => "Test", Quiet => True, Result => Result);
end;
```

---

## Conventions

### File Naming

- **Unit tests:** `test_<layer>_<component>.ads/adb`
  - Examples: `test_domain_value_person_name.ads`, `test_application_error.adb`

- **Integration tests:** `test_integration_<feature>.ads/adb`
  - Examples: `test_integration_create_greeting.ads`

- **E2E tests:** `test_e2e_<workflow>.ads/adb`
  - Examples: `test_e2e_greeting.ads`

- **Mocks:** `mock_<component>.ads/adb`
  - Examples: `mock_output_port.ads`

### Package Naming

Test packages follow Ada conventions:
- Package names match file names (underscores retained)
- Example: `package Test_Domain_Value_Person_Name`

### Test Organization Within Files

```ada
overriding procedure Run_Test (T : in out Test) is
begin
   --  Test 1: Component creation
   declare
      --  Setup
   begin
      --  Exercise and verify
      Assert (Condition, "Description");
   end;

   --  Test 2: Component behavior
   declare
      --  Setup
   begin
      --  Exercise and verify
      Assert (Condition, "Description");
   end;

   --  ... more tests
end Run_Test;
```

---

## Design Philosophy

This test suite demonstrates several key principles:

1. **Type-Based Organization** - Tests organized first by type (unit/integration/e2e), then by component
2. **Layer-Based Unit Tests** - Unit tests mirror architectural layers for clarity
3. **Shared Mocks** - Common test doubles in `common/mocks/` for reusability
4. **Explicit Test Types** - Clear distinction between unit, integration, and E2E tests
5. **Self-Documenting** - Test names and structure explain intent
6. **Educational Value** - Demonstrates professional Ada testing practices

---

## Best Practices Followed

### From abohlib/adafmt Projects

- ✅ Type-based primary organization (unit/integration/e2e)
- ✅ Separate test harness in `src/`
- ✅ Shared utilities in `common/`
- ✅ Consistent naming patterns
- ✅ Clear test documentation

### From AUnit Documentation

- ✅ Simple_Test_Cases for straightforward tests
- ✅ Test suite aggregation pattern
- ✅ Proper test organization
- ✅ Clear test output formatting

### From Clean Architecture

- ✅ Tests mirror architectural layers
- ✅ Dependency direction enforced (tests depend on production, not vice versa)
- ✅ Tests isolated in separate `/tests` directory
- ✅ Mock objects for dependency inversion

---

## Troubleshooting

### Build Issues

**Problem:** `cannot find source for Test_Domain_*`
**Solution:** Ensure tests.gpr Source_Dirs includes all test directories

**Problem:** `Mock_Output_Port not found`
**Solution:** Verify `tests/common/mocks` is in Source_Dirs

### Test Failures

**Problem:** Tests fail after refactoring
**Solution:** Run `alr clean` then rebuild: `alr exec -- gprbuild -P tests.gpr -p`

**Problem:** Mock not behaving as expected
**Solution:** Check mock implementation in `tests/common/mocks/`

---

## Educational Value

This test suite serves as a reference for:

1. **Professional Ada Testing** - Industry-standard practices
2. **Test Type Distinction** - Clear examples of unit/integration/E2E
3. **Mock Object Usage** - Proper dependency injection
4. **Test Organization** - Scalable structure for large projects
5. **AUnit Framework** - Effective use of Ada's testing framework

---

## Future Enhancements

Potential additions as the project grows:

- **Performance Tests** - Add `tests/performance/` for benchmarks
- **Property-Based Tests** - Add `tests/property/` for generative testing
- **Contract Tests** - Validate preconditions/postconditions
- **Fixtures Directory** - Add `tests/fixtures/` for simple examples
- **Separate GPR Files** - Split into `unit_tests.gpr`, `integration_tests.gpr`

---

## References

- [AUnit Documentation](https://docs.adacore.com/live/wave/aunit/html/aunit_cb/aunit_cb.html)
- [Ada 2022 Reference Manual](https://www.adaic.org/ada-resources/standards/ada22/)
- [Clean Architecture Principles](https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html)
- [Test Pyramid Pattern](https://martinfowler.com/articles/practical-test-pyramid.html)

---

**Last Updated:** 2025-10-24
**Status:** Production-Ready Educational Reference
