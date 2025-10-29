# Ada 2022 Hybrid Architecture Template Documentation

**Version:** 1.0.0
**Date:** October 28, 2025
**License:** BSD-3-Clause
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

Welcome to the documentation hub for the Ada 2022 Hybrid Architecture Template. This template provides a battle-tested foundation for building enterprise-grade Ada applications using proven architectural patterns combined with Ada's powerful type system and safety features.

## What You'll Learn

This documentation will help you understand:
- How to structure scalable Ada applications
- Why certain architectural decisions matter
- When to use different patterns and Ada features
- How to leverage Ada's type safety for better architecture
- How to avoid common pitfalls in both architecture and Ada

## Documentation Overview

### 🎯 Start Here

If you're new to the template, follow this path:

1. **[Quick Start Guide](../README.md)** - Get up and running with Alire
2. **[Architecture Overview](guides/architecture-overview.md)** - Understand the big picture
3. **[Domain Layer Guide](guides/domain-layer.md)** - Start with pure business logic

### 📚 Core Architecture Guides

Learn about each architectural layer and its responsibilities:

- **[Domain Layer Guide](guides/domain-layer.md)**
  The heart of your application - pure business logic with zero dependencies. Learn about value objects, domain services, and functional error handling with Result<T> and Either<L,R> monads.

- **[Application Layer Guide](guides/application-layer.md)**
  Orchestrates business logic and defines port interfaces. Understand use cases, dependency inversion, and the ports & adapters pattern.

- **[Infrastructure Layer Guide](guides/infrastructure-layer.md)**
  Connects your application to the outside world with concrete adapters. Learn about implementing ports, logging, concurrency, and error transformation.

- **[Presentation Layer Guide](guides/presentation-layer.md)**
  How users interact with your application through CLI, API, or GUI. Understand input validation, exit code mapping, and user experience.

- **[Bootstrap Module Guide](guides/bootstrap-module.md)**
  The composition root that wires everything together. Learn about dependency injection, signal handling, and application lifecycle management.

### 🔧 Functional Error Handling

Master Ada 2022's type-safe error handling with Result, Option, and Either:

- **[Functional Error Handling Guide](guides/functional-error-handling.md)**
  Deep dive into Result vs Either vs Option - when to use each type and why.

- **[Functional Types Tutorial](guides/functional-types-tutorial.md)**
  Comprehensive tutorial with examples for all functional types: Result<T,E>, Option<T>, Either<L,R>, and Try helpers.

- **[Functional Cheatsheet](guides/functional_cheatsheet.md)** ⭐
  One-page quick reference for common patterns - perfect for keeping handy while coding!

### 📊 Visual Architecture

UML diagrams to help visualize the system design:

#### System Overview
- **[Architecture Layers](diagrams/architecture-layers.svg)** - Five-layer dependency flow
- **[Package Structure](diagrams/package-structure.svg)** - Ada package organization with `with` dependencies
- **[Component View](diagrams/component-view.svg)** - Components and ports/adapters relationships

#### Domain Model
- **[Domain Model](diagrams/domain-model.svg)** - Result<T>, Either<L,R> monads, value objects, and domain services

#### Flow and Patterns
- **[Use Case Flow](diagrams/use-case-flow.svg)** - Sequence diagram showing request flow through all layers
- **[Error Handling](diagrams/error-handling.svg)** - Error types per layer and transformation strategy

### 🔄 For OOP Developers: Understanding Generics

Coming from Java, C#, or C++? These resources translate OOP patterns to Ada generics:

- **[From OOP to Ada Generics Guide](guides/oop-to-generics-guide.md)** ⭐
  Complete mental model translation from OOP interfaces to Ada generics. **Essential reading for OOP developers!**

#### Visual Translation Guides
- **[OOP vs Generic Comparison](diagrams/oop-vs-generic-comparison.svg)** - Side-by-side comparison of interface-based vs generic-based dependency injection
- **[Port-Adapter with Generics](diagrams/port-adapter-generic-pattern.svg)** - How the Port-Adapter pattern works with generics instead of interfaces
- **[Generic Architecture Layers](diagrams/generic-architecture-layers.svg)** - Complete layer structure showing generic instantiation flow
- **[Generic Instantiation Flow](diagrams/generic-instantiation-flow.svg)** - Sequence diagram showing compile-time composition

**Quick Translation Reference:**
- Interface → Generic formal parameter
- `implements` → Structural matching (duck typing at compile-time)
- Constructor injection → Generic instantiation
- Runtime polymorphism → Compile-time polymorphism (zero overhead!)

## Learning Path

### For OOP Developers New to Ada

If you're coming from Java, C#, C++, or similar OOP languages:

1. **Start Here**: [From OOP to Ada Generics Guide](guides/oop-to-generics-guide.md) - Understand how interfaces translate to generics
2. **Visual Learning**: Review the [OOP vs Generic diagrams](#-for-oop-developers-understanding-generics) above
3. **Then Proceed**: Continue with "Understanding Ada 2022 Architecture" below

### Understanding Ada 2022 Architecture

Start with understanding how Ada's features enable better architecture:

1. **Read**: [Architecture Overview](guides/architecture-overview.md) - See how GNAT Project files enforce architectural boundaries at compile-time
2. **Study**: [Domain Layer Guide](guides/domain-layer.md) - Learn how Ada's type system enables "make illegal states unrepresentable"
3. **Explore**: Package structure and dependency enforcement via `.gpr` files
4. **Build**: Run `alr build` and observe compile-time architecture checking

### Building Your First Feature

Follow these steps to add a new feature:

1. **Domain First**: Create value objects in `domain/src/value/`
   - Use smart constructors that return `Result`
   - Implement business rules in the Domain layer
   - Write unit tests (no mocking needed - pure functions!)

2. **Application Layer**: Define ports and use cases in `application/src/`
   - Create port interfaces for needed I/O operations
   - Implement use case that orchestrates domain logic
   - Write integration tests with mock adapters

3. **Infrastructure**: Implement adapters in `infrastructure/src/adapter/`
   - Create concrete types implementing port interfaces
   - Handle actual I/O operations
   - Test adapter behavior

4. **Presentation**: Add CLI handling in `presentation/src/cli/`
   - Parse user input
   - Call use cases
   - Map results to exit codes

5. **Bootstrap**: Wire dependencies in `bootstrap/src/`
   - Create concrete instances
   - Inject dependencies via `'Access`
   - Run application

### Mastering Concurrent Architecture

For concurrent applications:

1. **Study**: Protected objects in Infrastructure layer
2. **Implement**: Concurrent use cases using Ada tasks
3. **Test**: Verify thread safety with concurrent tests
4. **Deploy**: Use `Main_Concurrent` entry point

## Ada 2022 Features Used

This architecture leverages modern Ada 2022 capabilities:

- **`pragma Ada_2022;`** - All source files use Ada 2022 edition
- **Generic Packages** - `Result<T>` and `Either<L,R>` are generic for type safety
- **Interfaces** - Port definitions use abstract interface types
- **Tagged Types** - Adapters derive from interfaces for polymorphism
- **Tasks** - Concurrent use case implementations
- **Protected Types** - Thread-safe logging and state management
- **Contracts** - Pre/post conditions for design-by-contract
- **GPRbuild** - Enforces architectural dependencies at compile-time

## Common Questions

### Why This Architecture?

Traditional architectures often force trade-offs. This hybrid approach combines:
- **DDD**: Rich business modeling with value objects and domain services
- **Clean Architecture**: Clear boundaries enforced by GNAT Project files
- **Hexagonal**: Flexible ports & adapters pattern
- **Functional**: Result/Either monads for explicit error handling

Plus Ada's unique advantages:
- **Compile-time safety** - Architecture violations caught by compiler
- **Zero-cost abstractions** - Interfaces have no runtime overhead
- **Concurrency built-in** - Tasks and protected objects in the language
- **Strong typing** - Prevents many entire classes of bugs

### How Do I Start?

1. Install Alire: `https://alire.ada.dev/`
2. Clone the repository
3. Run `alr build` to build all layers
4. Run `./bootstrap/bin/hybrid-bootstrap-main Alice` to test
5. Explore the code structure in `domain/`, `application/`, etc.

### Where Can I Get Help?

- **Documentation**: Layer-specific guides in `/docs/guides/`
- **Examples**: Study the greeting use case implementation
- **Tests**: Learn from test organization in `tests/unit/`, `tests/integration/`

## Project Structure

```
hybrid/
├── domain/                      # Pure business logic (no dependencies)
│   ├── domain.gpr
│   └── src/
│       ├── value/               # Person_Name, etc.
│       ├── service/             # Greeting service
│       ├── model/               # Result<T>, Either<L,R>
│       └── error/               # Domain_Error enumeration
│
├── application/                 # Use cases and ports (depends on Domain)
│   ├── application.gpr
│   └── src/
│       ├── service/             # Create_Greeting use case
│       ├── port/                # Output_Port interface
│       └── error/               # Application_Error
│
├── infrastructure/              # Adapters (depends on Application + Domain)
│   ├── infrastructure.gpr
│   └── src/
│       ├── adapter/             # Console_Output adapter
│       ├── logger/              # Logging (sync/concurrent)
│       └── concurrent/          # Task coordination
│
├── presentation/                # UI layer (depends on Application)
│   ├── presentation.gpr
│   └── src/
│       └── cli/                 # CLI_Application
│
├── bootstrap/                   # Composition root (depends on ALL)
│   ├── bootstrap.gpr
│   └── src/
│       ├── hybrid-bootstrap-main.adb           # Synchronous entry
│       └── hybrid-bootstrap-main_concurrent.adb # Concurrent entry
│
├── tests/                       # Test suite
│   ├── unit/                    # Domain, Application unit tests
│   ├── integration/             # Cross-layer integration tests
│   └── e2e/                     # End-to-end tests
│
└── docs/                        # Documentation
    ├── guides/                  # Teaching-focused guides
    └── diagrams/                # UML diagrams (.puml + .svg)
```

## Best Practices

### Always Remember

1. **Domain First**: Start with pure business logic, no I/O
2. **Test Everything**: Domain tests are fast (no mocking!), write them first
3. **Use Ada's Type System**: Make illegal states unrepresentable
4. **Respect Dependencies**: Let GPRbuild enforce architectural rules
5. **Functional Errors**: Use Result<T>, not exceptions for business logic

### Common Mistakes to Avoid

1. **Skipping Layers**: Don't call Infrastructure from Presentation directly
2. **Primitive Obsession**: Use value objects, not plain `String` everywhere
3. **I/O in Domain**: Keep Domain pure - no `Ada.Text_IO` calls
4. **Ignoring Result**: Always pattern match on `Result` - don't ignore errors
5. **Circular Dependencies**: GPRbuild will catch this - respect the dependency flow

## Ada-Specific Patterns

### Smart Constructors with Result

```ada
-- Value object with validation
function Create (Name : String) return Result is
begin
   if Name'Length = 0 then
      return Failure (Empty_Person_Name);
   else
      return Success (Person_Name'(Value => To_Unbounded_String (Name)));
   end if;
end Create;
```

### Ports & Adapters with Interfaces

```ada
-- Application defines port
type Output_Port is interface;
procedure Write_Output (Self : Output_Port; Message : String) is abstract;

-- Infrastructure implements port
type Console_Output is new Output_Port with null record;
overriding procedure Write_Output (Self : Console_Output; Message : String);
```

### Manual Dependency Injection

```ada
-- Bootstrap wires concrete instances
Output_Adapter : aliased Console_Output.Console_Output_Adapter;
Use_Case : aliased Create_Greeting.Create_Greeting_Use_Case :=
             Create_Greeting.Create
               (Output_Port => Output_Adapter'Access);  -- Inject!
```

## Testing Strategy

The architecture enables comprehensive testing:

- **Unit Tests** (Domain): Pure functions, no mocking, fast
- **Integration Tests** (Application): Use cases with mock adapters
- **End-to-End Tests**: Full stack with real adapters
- **Architecture Tests**: GPRbuild enforces dependencies at compile-time

## Next Steps

Ready to dive deeper? Explore in order:

1. **[Architecture Overview](guides/architecture-overview.md)** - Complete system design
2. **[Domain Layer Guide](guides/domain-layer.md)** - Pure business logic
3. **[Application Layer Guide](guides/application-layer.md)** - Use cases and ports
4. **[Infrastructure Layer Guide](guides/infrastructure-layer.md)** - Adapters and I/O
5. **[Presentation Layer Guide](guides/presentation-layer.md)** - User interfaces
6. **[Bootstrap Module Guide](guides/bootstrap-module.md)** - Composition root

## References

- **Domain-Driven Design**: Eric Evans
- **Clean Architecture**: Robert C. Martin
- **Hexagonal Architecture**: Alistair Cockburn
- **Ada 2022 Reference**: [https://ada-lang.io/](https://ada-lang.io/)
- **Alire Package Manager**: [https://alire.ada.dev/](https://alire.ada.dev/)

## Contributing

Want to improve the documentation or template?

1. Fork the repository
2. Create a feature branch
3. Make your improvements
4. Ensure `alr build` and `alr exec -- gnatprove` succeed
5. Submit a pull request

Remember: Good documentation helps everyone learn faster!

---

*This documentation is a living guide. It grows with community contributions and real-world usage.*
