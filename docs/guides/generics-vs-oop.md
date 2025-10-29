# Generics vs OOP: A Practical Comparison for Dependency Injection

**Version:** 1.0.0
**Date:** October 28, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released


This guide provides a detailed comparison of two approaches to implementing hexagonal architecture and dependency injection in Ada:

1. **Object-Oriented Programming (OOP)** - Runtime polymorphism with interfaces and tagged types
2. **Generic Programming** - Compile-time polymorphism with generic formal parameters

Both achieve the same architectural goals (loose coupling, testability, maintainability), but through fundamentally different mechanisms.

---

## Table of Contents

- [Core Concepts](#core-concepts)
- [Port Definition](#port-definition)
- [Adapter Implementation](#adapter-implementation)
- [Service Implementation](#service-implementation)
- [Dependency Injection](#dependency-injection)
- [Trade-off Analysis](#trade-off-analysis)
- [When to Use Which](#when-to-use-which)

---

## Core Concepts

### OOP Approach: Runtime Polymorphism

**Key Idea**: Define abstract interfaces (abstract tagged types) and implement them through type extension. Dependencies are injected at runtime through access-to-class types.

```ada
-- Interface defines contract
type Output_Port is interface;
procedure Send (Self : Output_Port; Msg : String) is abstract;

-- Concrete implementation extends interface
type Console_Output is new Output_Port with null record;
overriding procedure Send (Self : Console_Output; Msg : String);

-- Runtime binding through access types
type Output_Port_Access is access all Output_Port'Class;
Service_Instance.Set_Output (Console_Output_Instance'Access);
```

**Characteristics**:
- Binding happens at **runtime**
- Uses **vtable dispatch** for method calls
- **Heap allocation** for access-to-class types
- Can swap implementations **dynamically**
- Dependencies hidden in implementation

### Generics Approach: Compile-Time Polymorphism

**Key Idea**: Define port signatures as generic formal parameters. Services are generic packages that get instantiated with concrete implementations. Dependencies are injected at compile-time through generic instantiation.

```ada
-- Port signature as generic formal
generic
   with function Send (Msg : String) return Result is <>;
package Application.Port.Output.API is
   -- Empty body - just a signature
end API;

-- Concrete function matches signature
package Infrastructure.Adapter.Console_Output is
   function Send (Msg : String) return Result;
end Console_Output;

-- Compile-time binding through instantiation
package Output_Port is new Application.Port.Output.API
  (Send => Console.Send);

package My_Service is new Application.Service.Create_Greeting.API
  (Output_Port => Output_Port, ...);
```

**Characteristics**:
- Binding happens at **compile-time**
- Uses **direct function calls** (no dispatch)
- **Stack-based** allocation
- Cannot swap implementations at runtime
- Dependencies explicit in specification

---

## Port Definition

### OOP: Abstract Interface

```ada
-- application/src/port/hybrid-application-port-output.ads
pragma Ada_2022;

package Hybrid.Application.Port.Output is

   -- Abstract interface defines the contract
   type Output_Port is interface;

   -- Abstract operations
   procedure Send
     (Self : Output_Port;
      Msg  : String) is abstract
   with Pre'Class  => Msg'Length > 0,
        Post'Class => True;

   -- Can also define functions
   function Is_Connected (Self : Output_Port) return Boolean is abstract;

end Hybrid.Application.Port.Output;
```

**Pros**:
- ✅ Simple, familiar syntax
- ✅ Easy to understand for OOP developers
- ✅ Can add default implementations
- ✅ Can extend with additional operations

**Cons**:
- ❌ Runtime overhead (vtable lookup)
- ❌ Requires heap allocation for access types
- ❌ Cannot enforce Result<T,E> return types generically
- ❌ Harder to compose multiple port constraints

### Generics: Port Signature

```ada
-- application/src/port/hybrid-application-port-output.ads
pragma Ada_2022;
with Ada.Strings.Unbounded;
with Hybrid.Domain.Foundation.Ports.Result_Port;

package Hybrid.Application.Port.Output is

   generic
      type App_Error is private;

      with package R is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T      => Ada.Strings.Unbounded.Unbounded_String,
           E      => App_Error,
           others => <>);

      -- Port signature as generic formal function
      with function Send (Msg : String) return R.Result is <>;

   package API is
      -- Empty body - this package is just a signature!
      -- The real "implementation" is the generic formal parameters.
   end API;

end Hybrid.Application.Port.Output;
```

**Pros**:
- ✅ Zero runtime overhead
- ✅ Enforces Result<T,E> return types
- ✅ Compile-time type checking
- ✅ Can compose multiple constraints easily
- ✅ Default parameter support (`is <>`)

**Cons**:
- ❌ More complex syntax
- ❌ Steeper learning curve
- ❌ Cannot add default implementations
- ❌ Requires understanding of formal parameters

---

## Adapter Implementation

### OOP: Tagged Type Extension

```ada
-- infrastructure/src/adapter/hybrid-infrastructure-adapter-console_output.ads
pragma Ada_2022;
with Hybrid.Application.Port.Output;

package Hybrid.Infrastructure.Adapter.Console_Output is

   -- Concrete adapter extends interface
   type Console_Adapter is new
     Hybrid.Application.Port.Output.Output_Port with null record;

   -- Implement abstract operations
   overriding procedure Send
     (Self : Console_Adapter;
      Msg  : String);

   overriding function Is_Connected
     (Self : Console_Adapter) return Boolean;

end Hybrid.Infrastructure.Adapter.Console_Output;
```

```ada
-- infrastructure/src/adapter/hybrid-infrastructure-adapter-console_output.adb
with Ada.Text_IO;

package body Hybrid.Infrastructure.Adapter.Console_Output is

   overriding procedure Send
     (Self : Console_Adapter;
      Msg  : String) is
   begin
      Ada.Text_IO.Put_Line (Msg);
   end Send;

   overriding function Is_Connected
     (Self : Console_Adapter) return Boolean is (True);

end Hybrid.Infrastructure.Adapter.Console_Output;
```

**Key Points**:
- Uses `overriding` keyword for safety
- Must implement all abstract operations
- Can access `Self` parameter for state
- Simple, straightforward implementation

### Generics: Function Matching Signature

```ada
-- infrastructure/src/adapter/hybrid-infrastructure-adapter-console_output.ads
pragma Ada_2022;
with Ada.Strings.Unbounded;
with Hybrid.Domain.Foundation.Ports.Result_Port;

generic
   type App_Error is private;

   with package R is new
     Hybrid.Domain.Foundation.Ports.Result_Port
       (T      => Ada.Strings.Unbounded.Unbounded_String,
        E      => App_Error,
        others => <>);

   IO_Failure : App_Error;

package Hybrid.Infrastructure.Adapter.Console_Output is

   -- Concrete function matching port signature
   function Send (Msg : String) return R.Result;

end Hybrid.Infrastructure.Adapter.Console_Output;
```

```ada
-- infrastructure/src/adapter/hybrid-infrastructure-adapter-console_output.adb
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Hybrid.Infrastructure.Adapter.Console_Output is

   function Send (Msg : String) return R.Result is
   begin
      Ada.Text_IO.Put_Line (Msg);
      return R.Ok (To_Unbounded_String (Msg));
   exception
      when others =>
         return R.Err (IO_Failure);
   end Send;

end Hybrid.Infrastructure.Adapter.Console_Output;
```

**Key Points**:
- Adapter itself is generic!
- Function signature must match port exactly
- Returns Result<T,E> for error handling
- Stateless (no `Self` parameter)
- Must be instantiated before use

---

## Service Implementation

### OOP: Accepts Interface Reference

```ada
-- application/src/service/hybrid-application-service-create_greeting.ads
pragma Ada_2022;
with Hybrid.Application.Port.Output;
with Hybrid.Domain.Value.Person_Name;

package Hybrid.Application.Service.Create_Greeting is

   type Service is tagged private;

   -- Constructor
   function Create
     (Output : access Hybrid.Application.Port.Output.Output_Port'Class)
      return Service;

   -- Use case operation
   procedure Run
     (Self     : Service;
      Raw_Name : String);

private

   type Service is tagged record
      Output_Port : access Hybrid.Application.Port.Output.Output_Port'Class;
   end record;

end Hybrid.Application.Service.Create_Greeting;
```

```ada
-- application/src/service/hybrid-application-service-create_greeting.adb
package body Hybrid.Application.Service.Create_Greeting is

   function Create
     (Output : access Hybrid.Application.Port.Output.Output_Port'Class)
      return Service is
   begin
      return (Output_Port => Output);
   end Create;

   procedure Run
     (Self     : Service;
      Raw_Name : String) is
      Name : Person_Name.Person_Name;
   begin
      -- Validate name (details omitted)
      Name := Person_Name.Create (Raw_Name);

      -- Build greeting
      declare
         Greeting : constant String := "Hello, " & Person_Name.To_String (Name) & "!";
      begin
         -- Call port via dynamic dispatch
         Self.Output_Port.Send (Greeting);
      end;
   end Run;

end Hybrid.Application.Service.Create_Greeting;
```

**Key Points**:
- Service stores reference to port
- Uses dynamic dispatch (`Self.Output_Port.Send`)
- Can swap output at runtime
- Simple dependency injection pattern

### Generics: Generic Over Port

```ada
-- application/src/service/hybrid-application-service-create_greeting.ads
pragma Ada_2022;
with Ada.Strings.Unbounded;
with Hybrid.Domain.Foundation.Ports.Result_Port;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Application.Port.Output;

package Hybrid.Application.Service.Create_Greeting is

   generic
      type App_Error is private;
      type Person_Name_Error is private;

      with function Map_Error (E : Person_Name_Error) return App_Error is <>;

      with package R is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T => Ada.Strings.Unbounded.Unbounded_String, E => App_Error, others => <>);

      with package R_Name is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T => Person_Name.Person_Name, E => Person_Name_Error, others => <>);

      with package Name_Ops is new
        Hybrid.Domain.Value.Person_Name.Operations.API (R => R_Name);

      with package Output_Port is new
        Hybrid.Application.Port.Output.API
          (App_Error => App_Error, R => R, others => <>);

   package API is
      function Run (Raw_Name : String) return R.Result
      with Inline;
   end API;

end Hybrid.Application.Service.Create_Greeting;
```

```ada
-- application/src/service/hybrid-application-service-create_greeting.adb
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Hybrid.Application.Service.Create_Greeting is

   package body API is

      function Run (Raw_Name : String) return R.Result is
      begin
         -- Validate and create name
         declare
            Name_Result : constant R_Name.Result := Name_Ops.Create (Raw_Name);
         begin
            if R_Name.Is_Err (Name_Result) then
               -- Map domain error to app error
               return R.Err (Map_Error (R_Name.Unwrap_Err (Name_Result)));
            end if;

            -- Build greeting
            declare
               Name     : constant Person_Name.Person_Name := R_Name.Unwrap (Name_Result);
               Greeting : constant String := "Hello, " & Person_Name.To_String (Name) & "!";
            begin
               -- Call port (direct function call, not dispatch!)
               return Output_Port.Send (Greeting);
            end;
         end;
      end Run;

   end API;

end Hybrid.Application.Service.Create_Greeting;
```

**Key Points**:
- Service is a generic package
- All dependencies are generic formal parameters
- Port is bound at instantiation time
- Direct function calls (no dispatch overhead)
- Cannot swap output at runtime
- More complex specification

---

## Dependency Injection

### OOP: Runtime Constructor Injection

```ada
-- bootstrap/src/hybrid-bootstrap-main.adb
with Ada.Command_Line;
with Hybrid.Infrastructure.Adapter.Console_Output;
with Hybrid.Application.Service.Create_Greeting;

procedure Hybrid.Bootstrap.Main is

   -- Create concrete adapter
   Console : aliased Hybrid.Infrastructure.Adapter.Console_Output.Console_Adapter;

   -- Create service with injected dependency
   Greeting_Service : constant Hybrid.Application.Service.Create_Greeting.Service :=
     Hybrid.Application.Service.Create_Greeting.Create (Console'Access);

begin
   -- Run service
   Greeting_Service.Run ("Alice");

end Hybrid.Bootstrap.Main;
```

**Characteristics**:
- Simple constructor call
- Dependencies passed at runtime
- Can swap implementations dynamically
- Heap allocation required for access types

### Generics: Compile-Time Instantiation

```ada
-- bootstrap/src/hybrid-bootstrap-main.adb
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;
with Hybrid.Infrastructure.Adapter.Console_Output;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Application.Service.Create_Greeting;
with Hybrid.Application.Port.Output;

procedure Hybrid.Bootstrap.Main is

   -- Step 1: Define application error type
   type App_Error is (Bad_Input, IO_Error, System_Error);

   -- Step 2: Instantiate Result adapter
   package App_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Unbounded_String, E => App_Error);

   package Person_Name_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Person_Name.Person_Name, E => Person_Name.Person_Name_Error);

   -- Step 3: Instantiate Person_Name operations
   package Person_Name_Ops is new
     Person_Name.Operations.API (R => Person_Name_Result.Instance);

   -- Step 4: Instantiate Console adapter
   package Console_Out is new
     Hybrid.Infrastructure.Adapter.Console_Output
       (App_Error => App_Error, R => App_Result.Instance, IO_Failure => IO_Error);

   -- Step 5: Bind Console to Output port
   package Output_Port is new
     Hybrid.Application.Port.Output.API
       (App_Error => App_Error, R => App_Result.Instance, Send => Console_Out.Send);

   -- Step 6: Error mapping
   function Map_Person_Name_Error
     (E : Person_Name.Person_Name_Error) return App_Error is (Bad_Input);

   -- Step 7: Instantiate service with all dependencies
   package Greeting_Service is new
     Hybrid.Application.Service.Create_Greeting.API
       (App_Error         => App_Error,
        Person_Name_Error => Person_Name.Person_Name_Error,
        Map_Error         => Map_Person_Name_Error,
        R                 => App_Result.Instance,
        R_Name            => Person_Name_Result.Instance,
        Name_Ops          => Person_Name_Ops,
        Output_Port       => Output_Port);

   Result : App_Result.Instance.Result;

begin
   -- Run service (direct function call)
   Result := Greeting_Service.Run ("Alice");

   if App_Result.Instance.Is_Ok (Result) then
      Ada.Command_Line.Set_Exit_Status (0);
   else
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Hybrid.Bootstrap.Main;
```

**Characteristics**:
- Multi-step instantiation chain
- All binding at compile-time
- Cannot swap implementations at runtime
- No heap allocation needed
- Verbose but explicit

---

## Trade-off Analysis

### Performance

| Aspect | OOP | Generics |
|--------|-----|----------|
| **Method Call Overhead** | ❌ Indirect (vtable lookup) | ✅ Direct (can inline) |
| **Memory Allocation** | ❌ Heap (access types) | ✅ Stack |
| **Binary Size** | ✅ Single implementation | ❌ Multiple instantiations |
| **Initialization Time** | ✅ Fast (runtime binding) | ✅ None (compile-time) |

**Winner**: Generics for performance-critical code

### Flexibility

| Aspect | OOP | Generics |
|--------|-----|----------|
| **Runtime Swapping** | ✅ Yes (dynamic dispatch) | ❌ No (compile-time binding) |
| **Plugin Architecture** | ✅ Easy | ❌ Difficult |
| **Configuration** | ✅ Simple (runtime config) | ❌ Requires recompile |
| **Testing** | ✅ Easy (mock injection) | ⚠️ Requires test instantiations |

**Winner**: OOP for dynamic systems

### Type Safety

| Aspect | OOP | Generics |
|--------|-----|----------|
| **Compile-Time Checking** | ⚠️ Limited | ✅ Comprehensive |
| **Signature Matching** | ⚠️ Runtime errors possible | ✅ Compile-time errors only |
| **Error Messages** | ✅ Clear | ❌ Complex (nested generics) |
| **Refactoring Safety** | ⚠️ Runtime failures possible | ✅ Compile errors catch all |

**Winner**: Generics for safety (if you understand the error messages)

### Maintainability

| Aspect | OOP | Generics |
|--------|-----|----------|
| **Code Clarity** | ✅ Simple, familiar | ❌ Complex syntax |
| **Learning Curve** | ✅ Gentle | ❌ Steep |
| **Dependency Visibility** | ❌ Hidden in body | ✅ Explicit in spec |
| **Refactoring** | ⚠️ Can break at runtime | ✅ Breaks at compile-time |

**Winner**: Depends on team experience

### Testability

| Aspect | OOP | Generics |
|--------|-----|----------|
| **Mock Injection** | ✅ Simple (runtime) | ⚠️ Verbose (instantiation) |
| **Test Doubles** | ✅ Easy to create | ⚠️ Must match signature exactly |
| **Test Isolation** | ✅ Good | ✅ Excellent (no state) |
| **Setup Complexity** | ✅ Simple | ❌ Complex (many instantiations) |

**Winner**: OOP for ease, Generics for isolation

---

## When to Use Which

### Use OOP When:

1. **You need runtime polymorphism**
   - Plugin systems
   - Strategy pattern with many implementations
   - Configuration-driven adapter selection

2. **You have heterogeneous collections**
   - Array of different adapters
   - Lists of handlers implementing same interface
   - Composite patterns

3. **Team is unfamiliar with advanced Ada generics**
   - Simpler onboarding
   - More familiar to developers from other languages
   - Less cognitive overhead

4. **You need default implementations**
   - Common behavior across adapters
   - Template Method pattern
   - Mixins

### Use Generics When:

1. **Performance is critical**
   - Real-time systems
   - Zero-overhead abstractions required
   - Inlining across layer boundaries needed

2. **All types known at compile-time**
   - Fixed architecture
   - Known adapter types
   - No dynamic loading

3. **Maximum type safety required**
   - Safety-critical systems
   - Want compile-time guarantees
   - Prefer compile errors to runtime failures

4. **You want pure dependency injection**
   - All dependencies explicit in specs
   - No hidden coupling
   - Clear dependency graphs

### Hybrid Approach: Mix Both

You can use **both** in the same system:

```ada
-- Use generics for core architecture (performance-critical)
package Core_Service is new Application.Service.Create_Greeting.API (...);

-- Use OOP for plugin system (runtime flexibility)
type Plugin_Interface is interface;
procedure Execute (Self : Plugin_Interface; Data : String) is abstract;
```

**Example**:
- Core domain logic → Generics (performance, safety)
- Logging/observability → OOP (many implementations)
- Configuration → OOP (runtime loading)
- Adapters → Generics (compile-time binding)

---

## Summary

Both approaches achieve the same architectural goals (loose coupling, testability, dependency inversion), but through different mechanisms:

**OOP** = **Runtime flexibility** + **Simple syntax** + **Familiar patterns**

**Generics** = **Compile-time safety** + **Zero overhead** + **Explicit dependencies**

Choose based on your project's priorities:
- Need **performance**? → Generics
- Need **flexibility**? → OOP
- Need **both**? → Hybrid approach

The Hybrid project demonstrates the generics approach to show Ada's strengths in compile-time metaprogramming and zero-cost abstractions. For a production system, consider your specific constraints and team expertise.
