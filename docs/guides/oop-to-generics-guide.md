# From OOP to Ada Generics: A Translation Guide

**Version:** 1.0.0
**Date:** October 28, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released


This guide helps you translate your OOP mental model (interfaces, dynamic dispatch) to the generic-based architecture (formal parameters, static dispatch) used in this project.

## Key Diagrams

### 1. OOP vs Generic Port-Adapter Pattern
**File:** `../diagrams/oop-vs-generic-comparison.svg`

This is the **most important diagram** for understanding the translation. It shows:

**Left side (OOP approach):**
- Interface `IOutputPort` with `Write` method
- `CreateGreetingService` depends on the interface
- Multiple adapters (`ConsoleAdapter`, `FileAdapter`) implement the interface
- Runtime polymorphism via virtual method calls

**Right side (Generic approach):**
- Generic formal parameter `with function Write(...)` (the "port")
- Generic package `CreateGreeting_Spec` (the service)
- Concrete adapters provide the `Write` function
- Bootstrap instantiates the generic with the adapter
- Compile-time polymorphism via static dispatch

**Key Translation:**
```
Interface                  → Generic formal parameter
implements                 → Structural matching (duck typing)
Dependency injection       → Generic instantiation
Runtime binding            → Compile-time binding
vtable dispatch            → Direct function call (zero overhead)
```

### 2. Port-Adapter Pattern with Generics
**File:** `../diagrams/port-adapter-generic-pattern.svg`

Shows the detailed translation of the Port-Adapter pattern:

**OOP Mental Model:**
```ada
-- Interface (port)
type Output_Port is interface;
procedure Write (Port : Output_Port; Message : String);

-- Use case depends on port
type Use_Case (Port : access Output_Port) is ...

-- Adapter implements port
type Console_Adapter is new Output_Port with null record;
overriding procedure Write (Adapter : Console_Adapter; Message : String);

-- Injection
My_Use_Case : Use_Case (Console_Adapter'Access);
```

**Generic Translation:**
```ada
-- Generic port (formal parameter)
generic
   with function Write (Message : String) return Result;
package Use_Case_Spec is
   procedure Execute;
end Use_Case_Spec;

-- Adapter provides matching function
package Console_Adapter is
   function Write (Message : String) return Result;
end Console_Adapter;

-- Instantiation (injection happens here)
package My_Use_Case is new Use_Case_Spec (
   Write => Console_Adapter.Write
);
```

**Key insight:** The generic formal parameter IS the port. The adapter just needs to provide a function that matches the signature.

### 3. Generic-Based Hybrid Architecture
**File:** `../diagrams/generic-architecture-layers.svg`

Shows the complete layer structure:

- **Domain Layer:** Pure business logic, generic over value operations
- **Application Layer:** Use case orchestration, generic over domain services and output ports
- **Infrastructure Layer:** Concrete implementations (adapters)
- **Bootstrap Layer:** Composition root where all generics are instantiated

**Flow:**
1. Domain defines generic services (e.g., `Greeting_Service_Spec`)
2. Application defines generic use cases (e.g., `CreateGreeting_Spec`)
3. Infrastructure provides concrete implementations (e.g., `Console.Write`)
4. Bootstrap instantiates everything with concrete types

### 4. Generic Instantiation Flow
**File:** `../diagrams/generic-instantiation-flow.svg`

Shows the sequence of instantiations at compile-time:

1. **Step 1:** Bootstrap instantiates domain service with concrete operations
2. **Step 2:** Bootstrap instantiates application service with domain service functions
3. **Step 3:** Runtime execution uses statically-bound calls (no virtual dispatch)

**Critical insight:** All instantiation happens at compile-time. At runtime, you just call the instantiated package's procedures/functions with zero overhead.

## Understanding Generics: Mental Model Translation

### From OOP to Generics

| OOP Concept | Generic Equivalent | Example |
|-------------|-------------------|---------|
| Interface | Generic formal parameter | `with function Write(...)` |
| Class implements interface | Function matches signature | `Console.Write` matches port signature |
| Constructor injection | Generic instantiation | `new Service_Spec(Write => Console.Write)` |
| Virtual method call | Direct function call | No vtable, compiler inlines if possible |
| Runtime polymorphism | Compile-time polymorphism | Type-safe at compile time |
| Dependency inversion | Generic abstraction | Application generic over domain |

### Key Differences

**OOP (Dynamic Dispatch):**
```ada
-- Define interface
type Output_Port is interface;
procedure Write (Port : Output_Port; Message : String);

-- Service depends on interface
type Service (Port : access Output_Port'Class) is ...

-- Runtime: vtable lookup for Write
Service.Execute;  -- Calls Port.Write via vtable
```

**Generics (Static Dispatch):**
```ada
-- Define port as generic parameter
generic
   with function Write (Message : String) return Result;
package Service_Spec is
   procedure Execute;
end Service_Spec;

-- Compile-time: direct call to Console.Write
package Service is new Service_Spec (Write => Console.Write);
Service.Execute;  -- Direct call, no vtable
```

## Benefits of the Generic Approach

1. **Zero Runtime Overhead:** No vtables, no indirection, compiler can inline
2. **Compile-Time Safety:** All dependencies verified at instantiation time
3. **No Heap Allocation:** No need for access types or dynamic allocation
4. **Better Optimization:** Compiler sees the full call chain
5. **Explicit Dependencies:** All dependencies visible in generic parameters

## How to Read the Code

When you see code like this in `bootstrap/src/hybrid-bootstrap-main.adb`:

```ada
package App_Service is new
  Hybrid.Application.Service.Create_Greeting_Spec
    (Domain_Result  => Domain_Result,
     Greeting_Func  => Domain_Service.Create_Greeting,
     Output_Result  => App_Result,
     Output_Func    => Console_Out.Write);
```

Think of it as:

```
App_Service = new Use_Case(
  dependencies = {
    create_greeting: Domain_Service.Create_Greeting,
    write_output:    Console_Out.Write
  }
)
```

But instead of runtime injection, the compiler:
1. Verifies types match
2. Generates specialized code
3. Creates direct function calls
4. Optimizes away the abstraction

## Where to Start

1. **Read:** `oop-vs-generic-comparison.svg` - Understand the core translation
2. **Study:** `port-adapter-generic-pattern.svg` - See the pattern in detail
3. **Explore:** `generic-architecture-layers.svg` - See how layers connect
4. **Trace:** `generic-instantiation-flow.svg` - Follow the compile-time composition

## Common "Aha!" Moments

**"Where's the interface?"**
→ The generic formal parameter IS the interface. Example: `with function Write(...)` defines the contract.

**"How do I swap implementations?"**
→ Create a different instantiation in Bootstrap with a different adapter.

**"Isn't this just templates?"**
→ Yes, but Ada generics are checked at definition time (unlike C++), so you get early error detection.

**"What about runtime configuration?"**
→ You can instantiate multiple versions and select at runtime:
```ada
package Console_Service is new Service_Spec (Console.Write);
package File_Service is new Service_Spec (File.Write);

-- Runtime selection
case Config is
   when Use_Console => Console_Service.Execute;
   when Use_File    => File_Service.Execute;
end case;
```

**"How do I test with mocks?"**
→ Create a mock adapter and instantiate the service with it:
```ada
package Mock_Output is
   function Write (Message : String) return Result;
end Mock_Output;

package Service_Under_Test is new Service_Spec (
   Write => Mock_Output.Write
);
```

## Next Steps

- Review the actual code in `bootstrap/src/hybrid-bootstrap-main.adb` with these diagrams open
- Trace a single use case from Bootstrap → Application → Domain → Infrastructure
- Compare to an OOP implementation you're familiar with
- Experiment: try creating a new adapter and instantiating with it

## Questions?

If anything is still unclear, review the diagrams in this order:
1. OOP vs Generic comparison (mental model)
2. Port-Adapter pattern (detailed translation)
3. Architecture layers (system structure)
4. Instantiation flow (how it works at compile-time)
