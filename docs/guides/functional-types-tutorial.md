# Functional Error-Handling Primitives in Ada 2022 — Tutorial

**Version:** 1.0.0  
**Date:** October 28, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  

## Overview

This tutorial shows how to use the **Result**, **Option**, **Either**, and **Try** helpers provided in the `Hybrid.Domain.Model.*` packages for functional error handling in Ada 2022.

### Four Core Types

- **`Hybrid.Domain.Model.Option.Generic (T)`** → Option<T>
- **`Hybrid.Domain.Model.Result.Generic (T, E)`** → Result<T, E>
- **`Hybrid.Domain.Model.Either.Generic (L, R)`** → Either<L, R>
- **`Hybrid.Domain.Model.Try_To_Result.Generic (...)`** → catch exceptions and convert to Result

### Key Features

- **Pure value types** with variant records
- **Expression functions** & contracts (Pre/Post conditions)
- **Railway-oriented programming** with combinators: `Map`, `And_Then`, `Map_Err`, `Or_Else`, etc.
- **Exception boundaries** - `Try_*` helpers keep exceptions local and turn them into typed errors
- **Zero overhead** - All abstractions compile to efficient Ada code

### File Organization

```
domain/src/model/
├── hybrid-domain-model.ads                    # Root package
├── hybrid-domain-model-option.ads/.adb        # Option<T>
├── hybrid-domain-model-result.ads/.adb        # Result<T,E>
├── hybrid-domain-model-either.ads/.adb        # Either<L,R>
├── hybrid-domain-model-try_to_result.ads/.adb # Exception → Result
├── hybrid-domain-model-try_to_option.ads/.adb # Exception → Option
├── hybrid-domain-model-try_map.ads/.adb       # Try + Map
└── hybrid-domain-model-catch_only.ads/.adb    # Selective exception handling
```

---

## Setup

Import the packages you need:

```ada
pragma Ada_2022;

with Hybrid.Domain.Model.Result;
with Hybrid.Domain.Model.Option;
with Hybrid.Domain.Model.Either;
with Hybrid.Domain.Model.Try_To_Result;
with Hybrid.Domain.Model.Try_To_Option;
with Hybrid.Domain.Model.Try_Map;
with Ada.Exceptions;
```

Define a rich error type (better than plain strings):

```ada
type Error_Kind is (IO_Error, Parse_Error, Validation_Error, Timeout, Unexpected);

type Error is record
   Kind    : Error_Kind;
   Message : String;
end record;
```

Instantiate `Result` and `Option` for common types:

```ada
package Str_Result  is new Hybrid.Domain.Model.Result.Generic (T => String, E => Error);
package Int_Result  is new Hybrid.Domain.Model.Result.Generic (T => Integer, E => Error);
package Str_Option  is new Hybrid.Domain.Model.Option.Generic (T => String);
package Int_Option  is new Hybrid.Domain.Model.Option.Generic (T => Integer);
```

---

## 1) Result<T,E> — The Workhorse for Error Handling

**Use Result for**: Fallible operations (I/O, parsing, validation, database calls)

### Basic Constructors and Predicates

```ada
declare
   R1 : constant Str_Result.Result := Str_Result.Ok ("Success!");
   R2 : constant Str_Result.Result := Str_Result.Err ((Kind => Parse_Error, Message => "Invalid integer"));
begin
   if Str_Result.Is_Ok (R1) then
      Put_Line (Str_Result.Value (R1));  -- "Success!"
   end if;

   if Str_Result.Is_Err (R2) then
      Put_Line ("Error: " & Str_Result.Error (R2).Message);
   end if;
end;
```

### Map - Transform Success Value

```ada
-- Map: Result<String,E> -> Result<Integer,E>
function Parse_Int (S : String) return Integer is (Integer'Value (S));

package MapStrToInt is new Str_Result.Map
  (U        => Integer,
   U_Result => Int_Result,
   F        => Parse_Int);

-- Usage:
declare
   R : constant Str_Result.Result := Str_Result.Ok ("42");
   I : constant Int_Result.Result := MapStrToInt.Map (R);  -- Ok(42)
begin
   null;
end;
```

### And_Then (Bind) - Chain Fallible Operations

```ada
-- And_Then (bind): String -> Result<Integer,Error>
function Parse_Int_Res (S : String) return Int_Result.Result is
begin
   return Int_Result.Ok (Integer'Value (S));
exception
   when Constraint_Error =>
      return Int_Result.Err ((Parse_Error, "Not an integer: " & S));
end Parse_Int_Res;

-- Instantiate And_Then:
package BindStrToInt is new Str_Result.And_Then (F => Parse_Int_Res);

-- Usage:
declare
   R : constant Str_Result.Result := Str_Result.Ok ("123");
   I : constant Int_Result.Result := BindStrToInt.And_Then (R);  -- Ok(123)
begin
   null;
end;
```

### Map_Err and With_Context - Enrich Errors

```ada
function Add_Ctx (E : Error; Msg : String) return Error is
begin
   return (Kind => E.Kind, Message => E.Message & " | context: " & Msg);
end Add_Ctx;

package AddCtx is new Str_Result.With_Context (Append => Add_Ctx);

-- Usage:
declare
   R : constant Str_Result.Result := Str_Result.Err ((IO_Error, "File not found"));
   R2 : constant Str_Result.Result := AddCtx.With_Context (R, "reading config.txt");
   -- R2 = Err((IO_Error, "File not found | context: reading config.txt"))
begin
   null;
end;
```

### Or / Or_Else_Result - Fallback Strategies

```ada
-- Or: pick first success
declare
   A : constant Str_Result.Result := Str_Result.Err ((Validation_Error, "empty"));
   B : constant Str_Result.Result := Str_Result.Ok ("fallback");
   C : constant Str_Result.Result := Str_Result.Or (A, B);  -- Ok("fallback")
begin
   null;
end;

-- Or_Else_Result: lazy alternative (computed only if needed)
function Lazy_Default return Str_Result.Result is
begin
   return Str_Result.Ok ("computed fallback");
end Lazy_Default;

package LazyAlt is new Str_Result.Or_Else_Result (F => Lazy_Default);
-- LazyAlt.Or_Else_Result (A);  -- Only calls Lazy_Default if A is Err
```

### Bimap - Map Both Success and Error

```ada
function Upper (S : String) return String is (S & "!");
function To_Unexpected (E : Error) return Error is
   ((Kind => Unexpected, Message => "wrapped: " & E.Message));

package Str_Bimap is new Str_Result.Bimap
  (U         => String,
   F_E       => Error,
   U_F_Result => Str_Result,
   F         => Upper,
   G         => To_Unexpected);

-- Str_Bimap.Bimap (Ok("test")) -> Ok("test!")
-- Str_Bimap.Bimap (Err(...)) -> Err with "wrapped: ..." message
```

### Recover / Recover_With - Handle Errors into Success

```ada
-- Recover: Err -> T
function Default_Str (E : Error) return String is
begin
   return "<missing>";
end Default_Str;

package RecoverStr is new Str_Result.Recover (Handle => Default_Str);

-- RecoverStr.Recover (Err(...)) -> Ok("<missing>")
-- RecoverStr.Recover (Ok("x"))  -> Ok("x")

-- Recover_With: Err -> Result<T,E>
function Recover_With_Fallback (E : Error) return Str_Result.Result is
begin
   return Str_Result.Ok ("recovered");
end Recover_With_Fallback;

package RecoverWith is new Str_Result.Recover_With (Handle => Recover_With_Fallback);
```

### Ensure - Validate with Predicates

```ada
function Non_Empty (S : String) return Boolean is (S'Length > 0);
function To_Val_Err (S : String) return Error is
   ((Kind => Validation_Error, Message => "empty string"));

package EnsureStr is new Str_Result.Ensure
  (Pred     => Non_Empty,
   To_Error => To_Val_Err);

-- EnsureStr.Ensure (Ok(""))    -> Err(Validation_Error, "empty string")
-- EnsureStr.Ensure (Ok("abc")) -> Ok("abc")
```

### Tap - Side Effects Without Breaking the Chain

```ada
procedure Log_Ok (S : String) is
begin
   Put_Line ("[OK] " & S);
end Log_Ok;

procedure Log_Err (E : Error) is
begin
   Put_Line ("[ERR] " & E.Kind'Image & ": " & E.Message);
end Log_Err;

package TapStr is new Str_Result.Tap (On_Ok => Log_Ok, On_Err => Log_Err);

-- TapStr.Tap (R) -- Logs but returns R unchanged (for pipeline logging)
```

### Map2 - Combine Two Results

```ada
function Concat (A, B : String) return String is (A & B);

package M2 is new Str_Result.Map2
  (U        => String,
   V        => String,
   U_Result => Str_Result,
   V_Result => Str_Result,
   F        => Concat);

-- M2.Map2 (Ok("ab"), Ok("cd")) -> Ok("abcd")
-- M2.Map2 (Err(...), Ok("cd")) -> Err(...)  -- short-circuits on first error
```

### Apply - Applicative Pattern

```ada
type Fn is not null access function (S : String) return Integer;

function Len (S : String) return Integer is (S'Length);

declare
   package Fn_Result is new Hybrid.Domain.Model.Result.Generic (T => Fn, E => Error);

   RF : constant Fn_Result.Result := Fn_Result.Ok (Len'Access);
   RX : constant Str_Result.Result := Str_Result.Ok ("hello");
begin
   package Ap is new Str_Result.Apply
     (U        => Integer,
      Func     => Fn,
      U_Result => Int_Result,
      F_Result => Fn_Result);

   declare
      R : constant Int_Result.Result := Ap.Apply (RF, RX);  -- Ok(5)
   begin
      null;
   end;
end;
```

### To_Option, Sequence, Traverse

```ada
-- To_Option: drop error detail
declare
   R : constant Str_Result.Result := Str_Result.Ok ("value");
   O : constant Str_Option.Option := Str_Result.To_Option (R);  -- Some("value")
begin
   null;
end;

-- Sequence: Array of Result -> Result<Array>
-- Traverse: Map + Sequence
-- See package specs for full generic parameters
```

---

## 2) Option<T> — Value or Absence

**Use Option for**: Expected absence (cache miss, optional field, lookup that may not exist)

### Basic Usage

```ada
declare
   O1 : constant Int_Option.Option := Int_Option.Some (42);
   O2 : constant Int_Option.Option := Int_Option.None;
begin
   if Int_Option.Is_Some (O1) then
      Put_Line ("Value: " & Integer'Image (Int_Option.Get (O1)));
   end if;
end;
```

### Filter - Keep Value Only If Predicate Holds

```ada
function Even (X : Integer) return Boolean is (X mod 2 = 0);

package FilterEven is new Int_Option.Filter (Pred => Even);

-- FilterEven.Filter (Some(4))  -> Some(4)
-- FilterEven.Filter (Some(3))  -> None
-- FilterEven.Filter (None)     -> None
```

### Zip - Combine Two Options into a Pair

```ada
type Pair is record
   A, B : Integer;
end record;

function Make_Pair (A, B : Integer) return Pair is
   ((A => A, B => B));

package Int_Option2 is new Hybrid.Domain.Model.Option.Generic (T => Integer);
package Pair_Option is new Hybrid.Domain.Model.Option.Generic (T => Pair);

package Z is new Int_Option.Zip
  (U           => Integer,
   Pair        => Pair,
   U_Option    => Int_Option2,
   Make_Pair   => Make_Pair,
   Pair_Option => Pair_Option);

-- Z.Zip (Some(2), Some(3)) -> Some((A => 2, B => 3))
-- Z.Zip (Some(2), None)    -> None
```

### Contains / Contains_With

```ada
declare
   O : constant Int_Option.Option := Int_Option.Some (5);
begin
   if Int_Option.Contains (O, 5) then
      Put_Line ("Contains 5!");
   end if;
end;

-- Contains_With: custom equality
function Custom_Eq (L, R : Integer) return Boolean is (abs (L - R) < 2);

package ContainsWith is new Int_Option.Contains_With (Eq => Custom_Eq);
```

### To_Result - Convert Absence to an Error

```ada
declare
   O : constant Str_Option.Option := Str_Option.None;
   R : constant Str_Result.Result :=
      Str_Option.To_Result (O, When_None => (Validation_Error, "value required"));
   -- R = Err((Validation_Error, "value required"))
begin
   null;
end;
```

---

## 3) Either<L,R> — One of Two Typed Alternatives

**Use Either for**: Neutral disjunction (not necessarily success/failure)

**Note**: For error handling, prefer `Result` over `Either` for clarity.

### Basic Usage

```ada
package E is new Hybrid.Domain.Model.Either.Generic (Left_Type => Error, Right_Type => Integer);

declare
   E1 : constant E.Either := E.Value (42);                        -- Right (success)
   E2 : constant E.Either := E.Error ((Validation_Error, "x"));  -- Left (error)
begin
   null;
end;
```

### Bimap - Map Both Sides

```ada
function Up_E (Er : Error) return Error is
   ((Kind => Er.Kind, Message => "UP:" & Er.Message));

function Times2 (I : Integer) return Integer is (I * 2);

package EB is new E.Bimap
  (L2 => Error,
   R2 => Integer,
   E2 => E,
   FL => Up_E,
   FR => Times2);

-- EB.Bimap (Value(21))     -> Value(42)
-- EB.Bimap (Error(e))      -> Error with "UP:" prefix
```

### Swap - Interchange Left and Right

```ada
package Swapped is new Hybrid.Domain.Model.Either.Generic (Left_Type => Integer, Right_Type => Error);

package ES is new E.Swap (Swapped => Swapped);

-- ES.Swap (Value(42))  -> Swapped.Left(42)
-- ES.Swap (Error(e))   -> Swapped.Right(e)
```

### Fold - Eliminate Either to a Single Value

```ada
function Err_To_String (E : Error) return String is
   ("ERROR: " & E.Message);

function Int_To_String (I : Integer) return String is
   (Integer'Image (I));

package EFold is new E.Fold
  (U        => String,
   On_Left  => Err_To_String,
   On_Right => Int_To_String);

-- EFold.Fold (Value(42))  -> " 42"
-- EFold.Fold (Error(...)) -> "ERROR: ..."
```

### From_Result - Convert Result to Either

```ada
package FromRes is new E.From_Result (R_Result => Int_Result);

declare
   R : constant Int_Result.Result := Int_Result.Ok (42);
   E : constant E.Either := FromRes.From_Result (R);  -- Value(42)
begin
   null;
end;
```

---

## 4) Try Bridges — Catch Exceptions Locally

**Use Try for**: Wrapping exception-throwing code at boundaries

### Try_To_Result - Exception → Result

```ada
function Read_File return String is
begin
   -- May raise Name_Error, Use_Error, etc.
   raise Ada.IO_Exceptions.Name_Error with "File not found";
end Read_File;

function Map_Ex (Ex : Ada.Exceptions.Exception_Occurrence) return Error is
begin
   return (Kind    => IO_Error,
           Message => Ada.Exceptions.Exception_Information (Ex));
end Map_Ex;

package Read_Try is new Hybrid.Domain.Model.Try_To_Result.Generic
  (T             => String,
   E             => Error,
   T_Result      => Str_Result,
   Action        => Read_File,
   Map_Exception => Map_Ex);

declare
   R : constant Str_Result.Result := Read_Try.Run;
   -- R = Err((IO_Error, "...Name_Error: File not found..."))
begin
   if Str_Result.Is_Err (R) then
      Put_Line ("Caught and converted: " & Str_Result.Error (R).Message);
   end if;
end;
```

### Try_To_Option - Exception → None

```ada
package Read_Opt is new Hybrid.Domain.Model.Try_To_Option.Generic
  (T        => String,
   T_Option => Str_Option,
   Action   => Read_File);

declare
   O : constant Str_Option.Option := Read_Opt.Run;
   -- O = None (exception was swallowed)
begin
   null;
end;
```

### Try_Map - Try + Map in One Step

```ada
function Parse (S : String) return Integer is (Integer'Value (S));

package TryParse is new Hybrid.Domain.Model.Try_Map.Generic
  (T             => String,
   U             => Integer,
   E             => Error,
   U_Result      => Int_Result,
   Action        => Read_File,
   Map_Exception => Map_Ex,
   F             => Parse);

declare
   R : constant Int_Result.Result := TryParse.Run;
   -- Reads file, parses to Integer, returns Result<Integer,Error>
begin
   null;
end;
```

### Catch_Only - Handle Specific Exceptions

```ada
function Is_IO_Exception (Ex : Ada.Exceptions.Exception_Occurrence) return Boolean is
begin
   return Ada.Exceptions.Exception_Name (Ex) = "ADA.IO_EXCEPTIONS.NAME_ERROR";
end Is_IO_Exception;

package Catch_IO is new Hybrid.Domain.Model.Catch_Only.Generic
  (T             => String,
   E             => Error,
   T_Result      => Str_Result,
   Action        => Read_File,
   Map_Exception => Map_Ex,
   Is_Handled    => Is_IO_Exception);

-- Catch_IO.Run catches only Name_Error, re-raises others
```

---

## Design Philosophy

### Exception Boundaries

```
┌─────────────────────────────────────────────────┐
│  External Code (may throw exceptions)          │
│  ┌───────────────────────────────────────────┐ │
│  │ Try_To_Result / Try_To_Option boundary    │ │
│  └───────────────────────────────────────────┘ │
└─────────────────────────────────────────────────┘
          ▼
┌─────────────────────────────────────────────────┐
│  Your Code (exception-free with Result/Option)  │
│  ┌───────────────────────────────────────────┐ │
│  │ Map, And_Then, Map_Err, etc.              │ │
│  └───────────────────────────────────────────┘ │
└─────────────────────────────────────────────────┘
```

### Railway-Oriented Programming

```ada
-- Each operation returns Result, failures short-circuit automatically
R := Parse_Config (Path)
  .And_Then (Validate'Access)
  .Map (Apply_Defaults'Access)
  .With_Context ("loading user config")
  .Tap (Log_Config'Access);
```

### Type Safety

```ada
-- ✓ GOOD: Compiler forces you to handle both cases
if R.Is_Ok then
   Process (R.Value);
else
   Handle_Error (R.Error);
end if;

-- ✗ BAD: Runtime exception if you guess wrong
-- val := R.Value;  -- RAISES if R is Err!
```

---

## Best Practices

1. **Use Result for error handling**, not Either (clearer semantics)
2. **Keep exceptions at boundaries** - use Try_* to convert once
3. **Design rich Error types** - use records with Kind + Message, not just String
4. **Leverage Tap for logging** - doesn't break your pipeline
5. **Use Ensure for validation** - keeps validation pure and composable
6. **Prefer Option over null** - Option<T> is safer than nullable pointers

---

## Summary Cheat Sheet

| Type              | Use When                           | Constructors        | Common Combinators                    |
|-------------------|------------------------------------|---------------------|---------------------------------------|
| **Result<T,E>**   | Fallible operations                | `Ok(T)`, `Err(E)`   | `Map`, `And_Then`, `Map_Err`, `Or`    |
| **Option<T>**     | Expected absence (not an error)    | `Some(T)`, `None`   | `Map`, `And_Then`, `Filter`, `Or_Else`|
| **Either<L,R>**   | Neutral alternatives               | `Left(L)`, `Right(R)`| `Bimap`, `Fold`, `Swap`              |
| **Try_To_Result** | Exception boundaries               | N/A (helper)        | `Run`                                 |

---

## Next Steps

- Read the **[Domain Layer Guide](domain-layer.md)** to see Result/Option in action with value objects
- Explore **[Error Handling Guide](ada_either_result_guide.md)** for advanced patterns
- Review implementation in `domain/src/model/hybrid-domain-model-*.ad*` files

---

**Remember**: Keep exceptions at the edges, use Result/Option internally, and let the type system guide you to handle all cases!
