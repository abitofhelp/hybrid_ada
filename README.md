# Hybrid Architecture - Ada 2022 Reference Application

**Version:** 1.0.0
**Date:** October 28, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

---

## Why This Project Exists

**The Missing Guide for Generic-Based Architecture**

When learning hexagonal/clean architecture, you'll find countless examples using OOP interfaces and dynamic dispatch (Java, C#, TypeScript). But what about languages like Ada, Rust, or modern C++ that favor generics and static dispatch?

**This project fills that gap.**

We provide:
- ✅ **Complete working example** of hexagonal architecture using **generics instead of interfaces**
- ✅ **Comprehensive primer textbook** (mdBook) explaining the mental model translation from OOP to generics
- ✅ **Side-by-side comparisons** showing the same patterns in both paradigms
- ✅ **Educational diagrams** visualizing how generic instantiation replaces runtime polymorphism

**The primer has been completed and is currently under editorial review. It will be made available soon.**

**If you've been searching for "how to do dependency injection without interfaces" or "hexagonal architecture with generics" — you've found it.**

---

## Overview

An Ada 2022 reference application demonstrating professional software architecture patterns including Domain-Driven Design (DDD), Clean Architecture, and Hexagonal Architecture principles with comprehensive test coverage.

---

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Ada 2022 Features](#ada-2022-features)
- [Test Coverage](#test-coverage)
- [Build System](#build-system)
- [Execution Modes](#execution-modes)
- [Getting Started](#getting-started)
- [Project Structure](#project-structure)
- [Learning Objectives](#learning-objectives)

---

## Overview

This project serves as an **educational reference** for building production-quality Ada applications using modern architectural patterns. While the current implementation features a simple greeting application, the architecture demonstrates principles that scale to enterprise applications.

### Key Features

- **Hybrid Architecture** - DDD/Clean/Hexagonal Architecture with strict layer separation
- **Proactive Architecture Enforcement** - Compiler catches transitive dependency violations at build time (not runtime!)
- **Functional Programming** - Result/Either monads for type-safe error handling
- **No Exceptions Across Boundaries** - All errors returned as values
- **Ada 2022 Best Practices** - Aspects, contracts, expression functions
- **Comprehensive Test Suite** - 100% business logic coverage with AUnit
- **Concurrent Design** - Task-based architecture with supervisor pattern
- **Type Safety** - Strong typing throughout with value objects

### What You'll Learn

1. **Domain-Driven Design** - Value objects, entities, domain services, ubiquitous language
2. **Clean Architecture** - Dependency inversion, use cases, ports and adapters
3. **Hexagonal Architecture** - Primary/secondary ports, infrastructure adapters
4. **Compiler-Enforced Architecture** - GNAT compiler validates hexagonal architecture layer dependencies at build time
5. **Functional Error Handling** - Result/Either monads without exceptions
6. **Ada 2022 Patterns** - Generics, aspects, contracts, expression functions
7. **Concurrent Programming** - Protected objects, tasks, supervisor patterns
8. **Professional Testing** - Unit tests, integration tests, E2E tests with mocks

---

## Architecture

### Layered Structure

```
┌─────────────────────────────────────────────────────────────┐
│                    Bootstrap Layer                          │
│  • Main entry point (hybrid-bootstrap-main.adb)            │
│  • Application composition and wiring                       │
│  • Signal handling (SIGTERM, SIGINT)                       │
│  • Task supervision and lifecycle management                │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│                   Presentation Layer                        │
│  • CLI interface (Hybrid.Presentation.CLI)                 │
│  • Argument parsing and validation                          │
│  • User interaction and feedback                            │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│                  Application Layer                          │
│  • Use cases (Create_Greeting)                             │
│  • Ports (Output port interface)                            │
│  • Application errors                                       │
│  • Orchestration logic                                      │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│                    Domain Layer                             │
│  • Value Objects (Person_Name)                             │
│  • Domain Models (Result, Either)                           │
│  • Domain Services (Greeting service)                       │
│  • Domain Errors                                             │
│  • Business rules and invariants                            │
└──────────────────┬──────────────────────────────────────────┘
                   ▲
                   │
┌─────────────────────────────────────────────────────────────┐
│                Infrastructure Layer                         │
│  • Adapters (Console_Output adapter)                       │
│  • Signal handling (POSIX signals on macOS/Linux)          │
│  • Error transformation                                     │
│  • Functional utilities (Result/Either adapters)           │
└─────────────────────────────────────────────────────────────┘
```

### Design Principles

1. **Dependency Rule** - Dependencies only point inward (Infrastructure → Application → Domain)
2. **Port and Adapter Pattern** - Interfaces in Application, implementations in Infrastructure
3. **Single Responsibility** - Each layer has one reason to change
4. **Interface Segregation** - Specific interfaces rather than general-purpose ones
5. **Dependency Inversion** - Depend on abstractions, not concretions
6. **No Exceptions Across Boundaries** - Result types for all fallible operations
7. **Architecture Guard** - Automated validation prevents layer boundary violations (`make test` runs `scripts/arch_guard.py`)

### Architecture Enforcement

**Application.Result Facade Pattern**

The Presentation layer must never directly import Domain types. To enforce this while still providing Result<T,E> semantics, we use the **Application.Result facade**:

```ada
-- ❌ VIOLATION: Presentation importing Domain directly
with Hybrid.Domain.Foundation.Ports.Result_Port;  -- DON'T DO THIS

-- ✅ CORRECT: Presentation imports through Application facade
with Hybrid.Application.Result.Result_Port;       -- Use this instead
```

**How it works:**
1. `Hybrid.Application.Result.Result_Port` is a signature-only generic at the Application layer
2. Bootstrap instantiates it with concrete types from Infrastructure adapters
3. Presentation depends only on Application, never on Domain
4. Dependency flow remains clean: Presentation → Application → Domain

**Validation (Three-Layer Enforcement):**

1. **Compile-Time Enforcement** - GNAT compiler catches violations during build:
   - `Interfaces` attribute in `application.gpr` whitelists only public API packages
   - `--no-indirect-imports` flag prevents transitive dependency leaks
   - Compilation fails if Presentation tries to import Domain directly

2. **Script-Based Validation** - `scripts/arch_guard_comprehensive.py`:
   - Validates all 5 layers (Domain, Application, Infrastructure, Presentation, Bootstrap)
   - Enforces hexagonal architecture dependency rules
   - Integrated into `make test` and `make check-arch`
   - Returns exit code 2 for violations (CI/CD integration)

3. **Development Workflow**:
   - `make check-arch` - Standalone architecture validation
   - `make test` - Runs architecture checks before tests
   - `make build` - Compiler enforcement on every build

**Application.Types - Bounded String API Boundaries**

Public API boundaries use bounded strings for predictable memory behavior:

```ada
-- Application layer defines the bounded Message_Type (max 1024 chars)
with Hybrid.Application.Types;

-- Convert between String and bounded Message_Type
Msg : constant Types.Message_Type := Types.To_Message ("Hello, World!");
Str : constant String := Types.To_String (Msg);
```

**Benefits:**
- **Predictable Memory** - Stack-allocated, no heap fragmentation
- **No Dynamic Allocation** - Eliminates allocation failures in message handling
- **API Best Practice** - Clear maximum message sizes at compile time
- **Performance** - Faster than unbounded strings for typical use cases

**Usage Pattern:**
1. All public API boundaries (Create_Greeting, Console_Output, CLI) use `Message_Type`
2. Internal processing may use `Unbounded_String` for flexibility
3. Conversion at API boundaries via `To_Message` and `To_String` functions
4. Maximum message size: 1024 characters (defined in `Application.Types`)

### Package Naming Convention

All packages follow the `Hybrid.<Layer>.<Category>.<Name>` convention:

- `Hybrid.Domain.Value.*` - Value objects
- `Hybrid.Domain.Model.*` - Domain models and patterns
- `Hybrid.Domain.Service.*` - Domain services
- `Hybrid.Domain.Error.*` - Domain errors
- `Hybrid.Application.Service.*` - Use cases
- `Hybrid.Application.Port.*` - Port interfaces
- `Hybrid.Application.Error.*` - Application errors
- `Hybrid.Infrastructure.Adapter.*` - Infrastructure adapters
- `Hybrid.Presentation.CLI` - Presentation layer
- `Hybrid.Bootstrap` - Application bootstrap

---

## Ada 2022 Features

### 1. Generic Result Monad

**Location:** `domain/src/model/hybrid-domain-model-result.ads:6-13`

```ada
generic
   type Err_Type is private;
   type Ok_Type  is private;

   with function "=" (L, R : Err_Type) return Boolean is <>;
   with function "=" (L, R : Ok_Type)  return Boolean is <>;
package Hybrid.Domain.Model.Result
   with Pure
is
   type Result (Is_Ok : Boolean := False) is private;
   -- ... monadic operations: Map_Ok, And_Then, Or_Else
end Hybrid.Domain.Model.Result;
```

**Benefits:**
- Type-safe error handling without exceptions
- Compiler-enforced error checking
- Monadic combinators for functional composition
- Explicit success/failure in type system

### 2. Value Objects with Validation

**Location:** `domain/src/value/hybrid-domain-value-person_name.ads:14-23`

```ada
type Person_Name is private
   with Type_Invariant => Is_Valid (Person_Name);

function Create (Name : String) return Person_Name_Result.Result
   with Pre  => Name'Length <= Max_Name_Length,
        Post => (if Person_Name_Result.Is_Ok (Create'Result)
                 then Is_Valid (Person_Name_Result.Get_Ok (Create'Result)));
```

**DDD Pattern:**
- Encapsulated validation
- Immutable once created
- Self-validating
- Type-safe construction

### 3. Aspects Over Pragmas

**Throughout codebase:**

```ada
-- Preferred: Aspects
package Hybrid.Domain
   with Pure, SPARK_Mode => On
is
   -- ...
end Hybrid.Domain;

-- Avoided: Pragmas
-- pragma Pure (Hybrid.Domain);
```

### 4. Expression Functions

**Location:** `domain/src/model/hybrid-domain-model-result.ads:30-31`

```ada
function Is_Ok (Self : Result) return Boolean is (Self.Is_Ok);
function Is_Err (Self : Result) return Boolean is (not Self.Is_Ok);
```

**Benefits:**
- More concise than full function bodies
- Can appear in package specifications
- Better for simple accessor functions

### 5. Discriminated Records for State

**Location:** `domain/src/model/hybrid-domain-model-result.ads:18-26`

```ada
type Result (Is_Ok : Boolean := False) is record
   case Is_Ok is
      when True =>
         Ok_Value : Ok_Type;
      when False =>
         Err_Value : Err_Type;
   end case;
end record;
```

**Type Safety:**
- Cannot access wrong variant
- Discriminant makes state explicit
- Memory efficient (no space for unused variant)

---

## Test Coverage

### Overview

The test suite achieves **100% business logic coverage** across all critical paths using AUnit 24.0.0. While package coverage is 29% (9/31 packages), this reflects the focus on testing domain and application layers rather than infrastructure adapters.

### Test Statistics

- **Test Suites:** 11
- **Test Cases:** 73+ assertions
- **Pass Rate:** 100% (0 failures)
- **Lines of Test Code:** 1,650+
- **Test Organization:** Type-based structure (unit/integration/e2e) following Ada best practices

### Test Types Explained

This project demonstrates three distinct testing approaches:

1. **Unit Tests** (`tests/src/domain/`, `tests/src/application/`)
   - Test individual components in complete isolation
   - Use mocks/stubs for all dependencies
   - Fast execution, focused on single component behavior
   - Example: Testing Create_Greeting use case with Mock_Output_Port

2. **Integration Tests** (`tests/src/integration/`)
   - Test interaction between 2-3 adjacent layers
   - Use real implementations (no mocks) for tested layers
   - Verify data flows correctly across layer boundaries
   - Example: Testing Application + Infrastructure (use case with real Console_Output_Adapter)

3. **End-to-End Tests** (`tests/src/e2e/`)
   - Test complete stack from top to bottom
   - Wire up all real components (Domain → Application → Infrastructure → Presentation)
   - Validate entire user workflows
   - Example: Testing full greeting flow including CLI application

### Coverage by Layer

#### Domain Layer (100% Coverage)

| Package | Test Suite | Test Cases | Coverage |
|---------|------------|------------|----------|
| `Hybrid.Domain.Value.Person_Name` | `test_domain_value_person_name.adb` | 10 tests | ✅ 100% |
| `Hybrid.Domain.Model.Result` | `test_domain_model_result.adb` | 11 tests | ✅ 100% |
| `Hybrid.Domain.Model.Either` | `test_domain_model_either.adb` | 12 tests | ✅ 100% |
| `Hybrid.Domain.Error` | `test_domain_error.adb` | 7 tests | ✅ 100% |
| `Hybrid.Domain.Service.Greeting` | `test_domain_service_greeting.adb` | 4 tests | ✅ 100% |

**Domain Test Coverage:**
- Value object validation (empty, whitespace, length, characters)
- Result monad operations (Ok, Err, Map_Ok, And_Then, Or_Else)
- Either monad operations (Left, Right, Map_Left, Map_Right)
- Error constructors and equality
- Domain service business logic

#### Application Layer (100% Critical Paths)

| Package | Test Suite | Test Cases | Coverage |
|---------|------------|------------|----------|
| `Hybrid.Application.Error` | `test_application_error.adb` | 5 tests | ✅ 100% |
| `Hybrid.Application.Service.Create_Greeting` | `test_application_service_create_greeting.adb` | 6 tests | ✅ 100% |

**Application Test Coverage:**
- Use case dependency injection
- Valid input processing
- Domain validation propagation
- Error handling (empty names, too-long names)
- Quiet mode operation
- Multiple execution scenarios

#### Infrastructure Layer (Unit Tests)

| Package | Test Suite | Test Cases | Coverage |
|---------|------------|------------|----------|
| `Hybrid.Infrastructure.Adapter.Console_Output` | `test_infrastructure_adapter_console_output.adb` | 4 tests | ✅ 100% |

**Infrastructure Test Coverage:**
- Adapter creation and initialization
- Task-based writer availability
- Send operation with real task
- Clean shutdown procedures

#### Presentation Layer (Unit Tests)

| Package | Test Suite | Test Cases | Coverage |
|---------|------------|------------|----------|
| `Hybrid.Presentation.CLI` | `test_presentation_cli.adb` | 3 tests | ✅ 100% |

**Presentation Test Coverage:**
- CLI application creation with dependencies
- Dependency wiring validation
- Multiple application coexistence
- Immutability of application structure

#### Integration Tests (Layer Boundaries)

| Test Suite | Test Cases | Coverage |
|------------|------------|----------|
| `test_integration_create_greeting.adb` | 4 tests | ✅ Application + Infrastructure |

**Integration Test Coverage:**
- Application layer integration with real infrastructure adapters
- Port/adapter pattern validation (no mocks)
- Error propagation across layer boundaries
- Adapter state management and repeated operations
- **Key difference from unit tests:** Uses real Console_Output_Adapter instead of mocks
- **Key difference from E2E tests:** Does NOT test presentation or bootstrap layers

#### End-to-End Tests (Full Stack)

| Test Suite | Test Cases | Coverage |
|------------|------------|----------|
| `test_e2e_greeting.adb` | 4 tests | ✅ Full stack (Ada/AUnit) |
| `test_all_modes.py` | 18 tests | ✅ Both execution modes (Python/pytest) |

**E2E Test Coverage (Ada/AUnit):**
- Full stack integration: Domain → Application → Infrastructure → Presentation
- Valid input flow through all layers
- Error propagation from domain to application
- Value object integration with services
- CLI application wiring and dependency injection

**E2E Test Coverage (Python/pytest):**
- Behavioral equivalence testing for both sync and async modes
- CLI contract validation (help, version, greetings)
- Error handling verification (missing arguments, empty input)
- Signal handler integration (clean startup/shutdown)
- No-hanging regression tests (programs complete within timeout)
- Parameterized testing: one test definition runs against both binaries

### Test Infrastructure

**Test Framework:** AUnit 24.0.0

**Test Organization:**
```
tests/
├── tests.gpr                          # Test project file
├── README.md                          # Comprehensive test documentation
├── src/                              # Test harness and execution
│   ├── test_suite.ads/adb            # Test suite aggregator
│   └── test_runner.adb               # Main test entry point
├── unit/                             # Unit tests by architectural layer
│   └── src/
│       ├── domain/                   # Domain layer unit tests
│       │   ├── test_domain_value_person_name.ads/adb
│       │   ├── test_domain_model_result.ads/adb
│       │   ├── test_domain_model_either.ads/adb
│       │   ├── test_domain_error.ads/adb
│       │   └── test_domain_service_greeting.ads/adb
│       ├── application/              # Application layer unit tests
│       │   ├── test_application_error.ads/adb
│       │   └── test_application_service_create_greeting.ads/adb
│       ├── infrastructure/           # Infrastructure layer unit tests
│       │   └── test_infrastructure_adapter_console_output.ads/adb
│       └── presentation/             # Presentation layer unit tests
│           └── test_presentation_cli.ads/adb
├── integration/                      # Integration tests (layer boundaries)
│   └── src/
│       └── test_integration_create_greeting.ads/adb
├── e2e/                             # End-to-end tests (full stack)
│   └── src/
│       └── test_e2e_greeting.ads/adb
├── common/                           # Shared test utilities
│   └── mocks/                        # Mock implementations
│       ├── mock_output_port.ads      # Mock Output_Port for unit testing
│       └── mock_output_port.adb
├── obj/                              # Test build artifacts
└── bin/
    └── test_runner                   # Compiled test executable
```

**Mock Objects:**
- `Mock_Output_Port` - Shared test double for output port interface
- Located in `tests/common/mocks/` for reusability
- Demonstrates dependency injection and interface-based testing

### Running Tests

#### Ada Unit Tests (AUnit)

```bash
# Build tests
alr exec -- gprbuild -P tests.gpr -p

# Run test suite
./tests/bin/test_runner

# Expected output:
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

#### Python E2E Tests (pytest)

```bash
# Run E2E tests for both sync and async modes
python3 tests/e2e/test_all_modes.py

# Or with pytest for verbose output
python3 -m pytest tests/e2e/test_all_modes.py -v

# Expected output:
============================= test session starts ==============================
collected 18 items

test_all_modes.py::TestCLIContract::test_help_displays_usage[sync] PASSED
test_all_modes.py::TestCLIContract::test_help_displays_usage[async] PASSED
test_all_modes.py::TestCLIContract::test_version_shows_version[sync] PASSED
test_all_modes.py::TestCLIContract::test_version_shows_version[async] PASSED
test_all_modes.py::TestCLIContract::test_valid_name_produces_greeting[sync] PASSED
test_all_modes.py::TestCLIContract::test_valid_name_produces_greeting[async] PASSED
... (18 tests total)

============================== 18 passed in 1.22s ==============================
```

**Note:** Python E2E tests demonstrate behavioral equivalence between synchronous and asynchronous execution modes, proving both implementations honor the same CLI contract.

### Untested Packages

The following packages are intentionally untested as they are infrastructure utilities or will be replaced in production:

- `Hybrid.Infrastructure.Adapter.Console_Output` - Validated via E2E tests
- `Hybrid.Infrastructure.Concurrent` - Utility functions
- `Hybrid.Infrastructure.Task_Supervisor` - Will be replaced per use case
- `Hybrid.Infrastructure.Error_Transform` - Simple transformations
- `Hybrid.Presentation.CLI` - Validated via E2E tests
- `Hybrid.Bootstrap.*` - Application composition (validated via manual testing)

As an **educational/reference application**, the focus is on demonstrating testing patterns rather than achieving 100% package coverage. The current test suite provides comprehensive examples of:
- **Unit testing:** Value objects, monads, services with mocks
- **Integration testing:** Layer boundaries with real adapters
- **E2E testing:** Complete stack validation

---

## Build System

### Alire Package Manager

**Project Configuration:** `alire.toml`

```toml
name = "hybrid"
description = "Hybrid Architecture reference application (DDD/Clean/Hexagonal)"
version = "1.0.0"

[[depends-on]]
aunit = "^24.0.0"

[gpr-set-externals]
HYBRID_BUILD_MODE = "dev"
```

### Building

```bash
# Initialize (first time only)
alr update

# Build all layers
alr build

# Build specific layer
alr exec -- gprbuild -P domain/domain.gpr
alr exec -- gprbuild -P application/application.gpr

# Build with optimizations
alr build --release

# Clean build
alr clean
alr build
```

### Running

```bash
# Run with no arguments (should fail validation)
./bootstrap/bin/hybrid-bootstrap-main

# Run with valid name
./bootstrap/bin/hybrid-bootstrap-main Alice

# Run with invalid name (too long)
./bootstrap/bin/hybrid-bootstrap-main "$(printf 'A%.0s' {1..300})"
```

### Build Modes

**Development (default):**
- Full debugging symbols (-g)
- Assertions enabled (-gnata)
- Validity checking (-gnatVa)
- All warnings (-gnatwa)

**Release:**
- Optimizations (-O2)
- Inlining enabled
- Assertions disabled
- Reduced binary size

---

## Execution Modes

### Overview

The Hybrid Architecture Template demonstrates two execution modes to show architectural flexibility:

1. **Synchronous Mode** - Single-threaded execution with Text_IO logging
2. **Concurrent Mode** - Task-based execution demonstrating concurrency patterns

**Status (v1.0.0):**
- ✅ **Both modes fully implemented and tested**
- ✅ **Clean shutdown without hanging** (signal handlers work correctly)
- ✅ **Behavioral equivalence proven via E2E tests** (18 parameterized tests)

### Synchronous Mode

**Binary:** `hybrid-bootstrap-main`

The synchronous version executes all operations on a single thread. This is the **recommended mode** for:
- Simple CLI applications
- Learning and educational purposes
- Single-user applications
- Applications where deterministic execution order is desired

**Architecture:**
- **Main thread only** - No application task
- **Synchronous logging** - Direct `Ada.Text_IO` calls (no Logger task)
- **Signal handling** - POSIX signals (SIGINT, SIGTERM) using AbohLib
- **Clean exit** - All signal handlers properly uninstalled

**Example output:**
```bash
./bootstrap/bin/hybrid-bootstrap-main Alice
[2025-10-28 10:30:45] [INFO ] Starting hybrid-bootstrap-main
[2025-10-28 10:30:45] [INFO ] Signal handlers installed (SIGINT, SIGTERM)
Hello, Alice!
[2025-10-28 10:30:45] [INFO ] Application completed with exit code: OK
[2025-10-28 10:30:45] [INFO ] Signal handlers uninstalled
```

### Concurrent Mode

**Binary:** `hybrid-bootstrap-main_concurrent`

The concurrent version demonstrates task-based concurrency patterns. Use this mode to learn:
- Task creation and lifecycle management
- Rendezvous communication patterns
- Signal handling across multiple tasks
- Task exception handling

**Architecture:**
- **Main thread + Application_Runner task** - Demonstrates task coordination
- **Synchronous logging** - Text_IO (logging is not the concurrency focus)
- **Signal handling** - POSIX signals propagated to task
- **Rendezvous communication** - Entry points for Start and Get_Exit_Code
- **Clean exit** - Task waits for completion, returns exit code

**Example output:**
```bash
./bootstrap/bin/hybrid-bootstrap-main_concurrent Bob
[2025-10-28 10:31:12] [INFO ] Starting hybrid-bootstrap-main_concurrent (concurrent mode)
[2025-10-28 10:31:12] [INFO ] Signal handlers installed (SIGINT, SIGTERM)
Hello, Bob!
[2025-10-28 10:31:12] [INFO ] Application completed with exit code: OK
[2025-10-28 10:31:12] [INFO ] Signal handlers uninstalled
```

### Behavioral Equivalence

Both modes honor the **exact same CLI contract**, proven by parameterized E2E tests:

```python
@pytest.mark.parametrize("binary,mode", [
    ("./bootstrap/bin/hybrid-bootstrap-main", "sync"),
    ("./bootstrap/bin/hybrid-bootstrap-main_concurrent", "async"),
])
```

**Tests validate:**
- Help and version flags produce identical output
- Same exit codes for success/failure scenarios
- Same error messages for invalid input
- Signal handlers install and uninstall cleanly
- No hanging - all programs complete within timeout

This demonstrates the architectural principle: **"Good architecture lets you swap implementations. E2E tests prove it."**

### Building Both Modes

Both executables are built automatically:

```bash
alr build
# Produces:
# - bootstrap/bin/hybrid-bootstrap-main (synchronous)
# - bootstrap/bin/hybrid-bootstrap-main_concurrent (concurrent)
```

Configuration in `bootstrap/bootstrap.gpr`:
```ada
for Main use ("hybrid-bootstrap-main.adb", "hybrid-bootstrap-main_concurrent.adb");
```

### Signal Handling (POSIX)

Both modes use AbohLib's POSIX signal handler for graceful shutdown:

**Supported platforms:**
- ✅ macOS (Darwin) - Tested
- ✅ Linux - Supported
- ✅ FreeBSD - Supported
- ⚠️ Windows - Not implemented (stub with helpful error message)

**Signals handled:**
- SIGINT (Ctrl+C) - Graceful shutdown, exit code 130
- SIGTERM - Graceful shutdown, exit code 143

**Architecture:**
- Protected objects for thread-safe state (no task entries)
- Cancellation source pattern for coordinated shutdown
- Clean install/uninstall lifecycle

### Runtime Requirements

**Both modes require:**
- Ada tasking runtime (`for tasking use "native"`)
- pthread library (linked via `-lpthread`)

**Why pthread is required:**
- Signal handler uses POSIX sigaction API
- Protected objects require tasking runtime
- Even synchronous mode uses protected objects for signal state

---

## Getting Started

### Prerequisites

1. **Alire** - Ada package manager (https://alire.ada.dev)
2. **GNAT** - Ada compiler (installed via Alire)
3. **GPRbuild** - Ada build system (installed via Alire)

### Quick Start

```bash
# 1. Clone repository
git clone <repository-url>
cd hybrid

# 2. Initialize dependencies
alr update

# 3. Build application
alr build

# 4. Run application
./bootstrap/bin/hybrid-bootstrap-main "World"

# Expected output:
Hello, World!

# 5. Build and run tests
alr exec -- gprbuild -P tests.gpr -p
./tests/bin/test_runner

# Expected output:
11 tests run: 11 passed, 0 failed
```

---

## Project Structure

```
hybrid/
├── alire.toml                        # Alire configuration
├── hybrid.gpr                        # Root project file
├── shared_config.gpr                 # Shared compiler settings
├── README.md                         # This file
├── LICENSE                           # BSD-3-Clause license
│
├── domain/                           # Domain Layer (Enterprise Business Rules)
│   ├── domain.gpr                    # Domain project file
│   ├── config/
│   │   └── hybrid_domain_config.gpr  # Domain build configuration
│   └── src/
│       ├── hybrid-domain.ads         # Domain root package
│       ├── value/                    # Value Objects
│       │   └── hybrid-domain-value-person_name.ads/adb
│       ├── model/                    # Domain Models
│       │   ├── hybrid-domain-model-result.ads/adb
│       │   └── hybrid-domain-model-either.ads/adb
│       ├── service/                  # Domain Services
│       │   └── hybrid-domain-service-greeting.ads/adb
│       └── error/                    # Domain Errors
│           └── hybrid-domain-error.ads/adb
│
├── application/                      # Application Layer (Use Cases)
│   ├── application.gpr
│   ├── config/
│   │   └── hybrid_application_config.gpr
│   └── src/
│       ├── hybrid-application.ads
│       ├── service/                  # Use Cases
│       │   └── hybrid-application-service-create_greeting.ads/adb
│       ├── port/                     # Port Interfaces
│       │   └── hybrid-application-port-output.ads
│       └── error/                    # Application Errors
│           └── hybrid-application-error.ads/adb
│
├── infrastructure/                   # Infrastructure Layer (Adapters)
│   ├── infrastructure.gpr
│   ├── config/
│   │   └── hybrid_infrastructure_config.gpr
│   └── src/
│       ├── hybrid-infrastructure.ads
│       ├── adapter/                  # Port Implementations
│       │   └── hybrid-infrastructure-adapter-console_output.ads/adb
│       ├── hybrid-infrastructure-concurrent.ads/adb
│       ├── hybrid-infrastructure-task_supervisor.ads/adb
│       ├── hybrid-infrastructure-error_transform.ads/adb
│       └── logger/
│           └── hybrid-infrastructure-logger-console.ads/adb
│
├── presentation/                     # Presentation Layer (UI)
│   ├── presentation.gpr
│   ├── config/
│   │   └── hybrid_presentation_config.gpr
│   └── src/
│       ├── hybrid-presentation.ads
│       └── cli/
│           └── hybrid-presentation-cli.ads/adb
│
├── bootstrap/                        # Application Bootstrap
│   ├── bootstrap.gpr
│   ├── config/
│   │   └── hybrid_bootstrap_config.gpr
│   └── src/
│       ├── hybrid-bootstrap.ads/adb  # Bootstrap orchestration
│       ├── hybrid-bootstrap-main.adb # Main entry point
│       ├── hybrid-bootstrap-signals.ads/adb
│       └── hybrid-bootstrap-exit_code.ads
│
├── scripts/                          # Python automation scripts
│   ├── README.md                     # Scripts documentation
│   ├── common.py                     # Shared utilities (colors, OS detection)
│   ├── install_tools.py              # Install development dependencies
│   └── run_coverage.py               # Run tests with coverage analysis
│
└── tests/                            # Test Suite (separate from application)
    ├── tests.gpr                     # Test project file
    ├── README.md                     # Test documentation
    ├── src/                          # Test harness
    │   ├── test_suite.ads/adb        # Test aggregator
    │   └── test_runner.adb           # Test entry point
    ├── unit/                         # Unit tests by layer
    │   └── src/
    │       ├── domain/               # Domain layer unit tests
    │       ├── application/          # Application layer unit tests
    │       ├── infrastructure/       # Infrastructure layer unit tests
    │       └── presentation/         # Presentation layer unit tests
    ├── integration/                  # Integration tests
    │   └── src/
    ├── e2e/                          # E2E tests
    │   └── src/
    ├── common/                       # Shared test utilities
    │   └── mocks/                    # Mock implementations
    ├── obj/                          # Test objects
    └── bin/
        └── test_runner               # Test executable
```

---

## Learning Objectives

### Architecture Patterns

1. **Domain-Driven Design**
   - Value objects with invariants (`Person_Name`)
   - Domain services for business logic (`Greeting`)
   - Ubiquitous language in code

2. **Clean Architecture**
   - Layer independence (Domain has no dependencies)
   - Use cases orchestrate domain logic
   - Dependency inversion via ports

3. **Hexagonal Architecture**
   - Primary ports (CLI interface)
   - Secondary ports (Output port)
   - Adapters in infrastructure layer

### Ada 2022 Techniques

1. **Generic Programming**
   - Result monad with any error/value types
   - Either monad for dual-value returns
   - Reusable functional patterns

2. **Type Safety**
   - Value objects prevent invalid states
   - Discriminated records for variants
   - Strong typing throughout

3. **Error Handling**
   - No exceptions across boundaries
   - Result types for fallible operations
   - Explicit success/failure handling

4. **Contracts**
   - Preconditions on inputs
   - Postconditions on outputs
   - Type invariants on value objects

### Testing Practices

1. **Unit Testing**
   - Test each component in isolation
   - Mock dependencies via interfaces
   - Assert expected behaviors

2. **Integration Testing**
   - Test layer interactions
   - Verify data flow between components
   - Validate error propagation

3. **End-to-End Testing**
   - Test complete user workflows
   - Validate full stack integration
   - Ensure system meets requirements

---

## Next Steps

This is a **starter template** for educational purposes. The example greeting application will be replaced as you build your own domain model. Key areas to extend:

1. **Domain Layer** - Replace Person_Name/Greeting with your business entities
2. **Use Cases** - Add your application-specific use cases
3. **Adapters** - Implement adapters for your infrastructure (database, HTTP, etc.)
4. **Tests** - Follow the testing patterns for your new domain logic

The architecture patterns demonstrated here scale to enterprise applications while maintaining testability and maintainability.

---

## License

**BSD-3-Clause License**

Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.

See [LICENSE](LICENSE) file for full terms.

---

## Contact

**Author:** Michael Gardner
**Organization:** A Bit of Help, Inc.
**Email:** mjgardner@abitofhelp.com

---

*Last Updated: 2025-10-28*
*Status: v1.0.0 Pre-release - Production-ready hybrid architecture with both synchronous and concurrent execution modes*
