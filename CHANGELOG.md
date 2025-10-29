# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-10-28

**Initial Release** - Ada 2022 Hybrid Architecture Reference Application

### Overview

This release provides a complete, production-ready reference implementation of hexagonal/clean architecture using **Ada 2022 generics** instead of traditional OOP interfaces. This is the first comprehensive example and educational resource for implementing dependency injection and architectural patterns with static dispatch.

### Architecture & Design

**Core Architecture:**
- Hexagonal (Ports & Adapters) architecture with strict layer boundaries
- Generic-based dependency injection (zero runtime overhead)
- Clean Architecture principles (dependency inversion, use cases)
- Domain-Driven Design patterns (value objects, domain services)
- Functional error handling with Result/Either monads
- No exceptions across layer boundaries
- **Application.Result facade** - Eliminates Presentation→Domain coupling via Application layer abstraction
- **Application.Types bounded strings** - Predictable memory at API boundaries with 1024-char max
- **Three-layer architecture enforcement**:
  - Compiler enforcement via GPR `Interfaces` attribute + `--no-indirect-imports` flag
  - Script validation via `arch_guard_comprehensive.py` (all 5 layers)
  - Integrated into build and test workflows

**Layered Structure:**
- **Domain Layer** - Pure business logic with rich value objects (Person_Name)
- **Application Layer** - Use cases with port interfaces (Create_Greeting)
- **Infrastructure Layer** - Adapters (Console_Output, functional adapters for Result/Either/Option)
- **Presentation Layer** - CLI interface with argument parsing
- **Bootstrap Layer** - Application composition and wiring with generics
- **Shared Layer** - Common types, version management

### Features

**Error Handling:**
- Rich error types using Ada tagged records with context
- Type-safe error propagation through Result/Either monads
- Domain, Application, Infrastructure, and Presentation error hierarchies
- Error transformation at layer boundaries

**Testing:**
- Complete test coverage (10 comprehensive test suites)
- Unit tests for all business logic
- Integration tests for layer interactions
- Mocked adapters for isolated testing
- AUnit test framework integration
- **Comprehensive architecture validation**:
  - `scripts/arch_guard_comprehensive.py` validates all 5 layers
  - GNAT compiler enforcement via `--no-indirect-imports`
  - GPR `Interfaces` attribute whitelists public APIs
  - Integrated into `make test` and `make check-arch`

**Development Tooling:**
- Automated version synchronization across all Alire crates
- Ada version package generation from alire.toml
- Complete release automation (prepare, release, diagrams commands)
- Markdown header management
- Python scripts with consistent headers and licensing
- Comprehensive Makefile with 40+ targets

**Documentation:**
- Comprehensive primer textbook (mdBook) - under editorial review
- 12 PlantUML architecture diagrams with SVG exports
- Side-by-side OOP vs Generics comparison diagrams
- Educational guides for each architectural layer
- Complete API documentation in Ada source files
- Migration guide for OOP developers

### Technical Specifications

**Language & Standards:**
- Ada 2022 (pragma Ada_2022 throughout)
- Modern Ada features: aspects, contracts, expression functions, delta aggregates
- GNAT 15.1.2 compiler
- Alire package manager integration
- **Categorization pragmas** - Preelaborate aspects on root packages for proper elaboration
- **SPDX identifiers** - BSD-3-Clause headers on all project files (.ads, .adb, .gpr)

**Build System:**
- Alire for dependency management
- GPRbuild project files for each layer with `Interfaces` whitelisting
- Multiple build profiles (development, release, validation, optimize)
- Automated test running and coverage analysis
- `--no-indirect-imports` compiler flag enforces explicit dependencies

**Concurrency Infrastructure:**
- Signal handling infrastructure (SIGTERM, SIGINT)
- Protected objects for thread-safe operations
- Task-based concurrency patterns
- Note: POSIX signal integration deferred to v1.1+ (infrastructure complete)

### Known Limitations

**Post-1.0 Enhancements:**
- POSIX signal handlers work but task cleanup times out on exit (workaround: use timeout in tests)
- Concurrent main variant exists but needs refactoring for latest patterns
- Signal handling infrastructure complete but OS attachment deferred

### Python Automation Scripts

Located in `/scripts/` directory:
- `sync_versions.py` - Synchronize versions across all alire.toml files
- `generate_version.py` - Generate Ada version package from alire.toml
- `add_md_headers.py` - Maintain markdown file metadata
- `release.py` - Complete release orchestration (prepare, release, diagrams)
- `install_tools.py` - Install development dependencies
- `run_coverage.py` - Test coverage analysis with HTML reports
- `arch_guard.py` - Simple Presentation→Domain boundary validator (legacy)
- `arch_guard_comprehensive.py` - Complete 5-layer architecture validator with color-coded output
- `common.py` - Shared utilities for all scripts

### Educational Value

**What You'll Learn:**
- How to implement hexagonal architecture without OOP interfaces
- Generic-based dependency injection and inversion of control
- Mental model translation from OOP dynamic dispatch to static dispatch
- Functional error handling without exceptions
- Ada 2022 best practices and modern patterns
- Professional project structure and tooling

**Target Audience:**
- OOP developers learning Ada
- Rust/C++ developers using generics
- Anyone seeking alternatives to interface-based architecture
- Software architects exploring static dispatch patterns

### Project Statistics

- **Lines of Ada Code**: ~3,500 (source + tests)
- **Test Suites**: 9 comprehensive test suites
- **Architectural Layers**: 6 (Bootstrap, Presentation, Application, Domain, Infrastructure, Shared)
- **Python Scripts**: 7 automation tools
- **Documentation**: 15+ guides, 12 diagrams, complete primer textbook
- **Makefile Targets**: 40+ commands

### License & Copyright

- **License**: BSD-3-Clause
- **Copyright**: © 2025 Michael Gardner, A Bit of Help, Inc.
- **SPDX-License-Identifier**: BSD-3-Clause

---

## Future Enhancements (Post-1.0)

### Planned for v1.1+
- **POSIX Signal Integration**: Connect signal handler infrastructure to OS signals
  - Infrastructure complete, OS attachment deferred
  - See: `bootstrap/src/hybrid-bootstrap-signals.adb:219` for implementation notes
