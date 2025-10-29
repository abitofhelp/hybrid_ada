# Using `Either` and `Result` in Ada 2022 — A Practical Guide

**Version:** 1.0.0  
**Date:** October 28, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  


This guide shows how to model explicit, exception‑free control flow in Ada 2022 using two algebraic types:

- **`Result<Ok, Err>`** — for **fallible operations** (success vs error).
- **`Either<L, R>`** — for a **neutral disjunction** (“this or that” value) when it’s **not** specifically success vs error.

> The examples below assume you have packages named `Hybrid.Domain.Model.Either` and `Hybrid.Domain.Model.Result` (as in the earlier snippets).  
> If your package names differ, just adjust the `with`/`use` lines accordingly.

---

## Mental model (quick)

- **Result<Ok, Err>** — read as “returns Ok or Err.” Use it for parsing, I/O, validation, database calls, etc.
- **Either<L, R>** — read as “value is one of two alternatives.” Use it to unify branches or represent two shapes of data.
- Keep exceptions at the *edges* (interop with exception‑throwing libraries). Inside your code, prefer pure, explicit flows.

---

## 1) `Result` basics: success/error without exceptions

**Goal:** Parse an integer from text, validate it, add context on failure, and recover safely.

```ada
pragma Ada_2022;

with Ada.Text_IO;               use Ada.Text_IO;
with Hybrid.Domain.Model.Result;

procedure Demo_Result is
   -- Instantiate Result<String, Integer>
   package R is new Hybrid.Domain.Model.Result
     (Err_Type => String,
      Ok_Type  => Integer);
   use R;

   -- map Ok (+1)
   function Inc (X : Integer) return Integer is (X + 1);
   function Map_Inc is new Map_Ok (F => Inc);

   -- validate Ok (must be > 0) else Err("non-positive")
   function Is_Pos (X : Integer) return Boolean is (X > 0);
   function Ensure_Pos is new Ensure (Pred => Is_Pos);

   -- append context to Err: E ++ " | ctx=" & C
   function Add_Ctx (E, C : String) return String is (E & " | ctx=" & C);
   function With_Context is new Context
     (Ctx_Type => String, Add => Add_Ctx);

   -- recover on error with a fallback value
   function Recover (_ : String) return Result is (Ok (42));
   function On_Err is new Or_Else (F => Recover);

   -- Log taps (no mutation)
   procedure Log_Ok  (V : Integer) is
   begin
      Put_Line ("OK=" & Integer'Image (V));
   end;
   procedure Log_Err (E : String) is
   begin
      Put_Line ("ERR=" & E);
   end;
   function TapO is new Tap_Ok  (Proc => Log_Ok);
   function TapE is new Tap_Err (Proc => Log_Err);

   -- Safe parse using a boundary that *catches* exceptions and converts to Result
   function Parse_Int (S : String) return Result is
   begin
      return Ok (Integer'Value (S));
   exception
      when Constraint_Error =>
         return Err ("invalid integer: """ & S & """");
   end Parse_Int;

   A : Result := Parse_Int ("7");
   B : Result := Parse_Int ("oops");  -- will be Err(...)
begin
   -- A: Ok path → +1 → ensure positive → log
   A := TapO (Ensure_Pos (Map_Inc (A), On_Fail => "non-positive"));

   -- B: Err path → add context → log → recover to Ok(42)
   B := On_Err (TapE (With_Context (B, "parsing user input")));

   Put_Line ("A final = " & Integer'Image (Get_Ok (A)));
   Put_Line ("B final = " & Integer'Image (Get_Ok (B)));
end Demo_Result;
```

**What to notice**
- **No exceptions in core logic.** We only catch once at the boundary (`Parse_Int`) to convert into `Result`.
- `Map_Ok`, `Ensure`, `Context`, `Or_Else`, and `Tap_*` keep flows readable and explicit.

---

## 2) `Either` basics: model two shapes of data

**Goal:** Represent “number or text,” transform each side, then fold into a single printable string.

```ada
pragma Ada_2022;

with Ada.Text_IO;        use Ada.Text_IO;
with Hybrid.Domain.Model.Either;

procedure Demo_Either is
   -- Either<String, Integer>
   package E is new Hybrid.Domain.Model.Either (Left_Type => String, Right_Type => Integer);
   use E;

   -- Builders
   N : Either := Right (10);             -- holds Integer
   T : Either := Left  ("ten");          -- holds String

   -- Map Right (+1)
   function Inc (X : Integer) return Integer is (X + 1);
   function Map_R is new Map (F => Inc);

   -- Map Left (add "!")
   function Up (S : String) return String is (S & "!");
   function Map_L is new Map_Left (F => Up);

   -- Fold to a single String
   function Show is new Fold
     (T        => String,
      On_Left  => (function (S : String)  return String is ("S: " & S)),
      On_Right => (function (V : Integer) return String is ("N: " & Integer'Image (V))));

begin
   Put_Line (Show (Map_R (N)));  -- "N:  11"
   Put_Line (Show (Map_L (T)));  -- "S: ten!"
end Demo_Either;
```

**When to use `Either`**
- You truly have **two alternatives** that are **not** “success vs error.”
- You want to unify code paths (e.g., two different calculation strategies) and then **fold** them to a common result.

---

## 3) Choosing between `Result` and `Either`

**Pick `Result` when:**
- Modeling **fallible operations** (I/O, parsing, validation, DB calls).
- You want error‑centric combinators (`Map_Err`, `Context`, `Or_Else`, `Ensure`).
- You want call sites to scream “this can fail.”

**Pick `Either` when:**
- You’re modeling a **neutral disjunction** of data shapes (e.g., `String` *or* `Integer`).
- You’re factoring **branchy pipelines** into a single flow and finishing with a **Fold**.

> Tip: If you catch yourself using `Either` to represent success/failure, it’s usually clearer to switch to `Result`.

---

## 4) Common recipes

### A) Validating a record with multiple checks (`Result`)

```ada
pragma Ada_2022;

with Hybrid.Domain.Model.Result;

procedure Demo_Validate is
   type User is record
      Name : String (1 .. 16);
      Age  : Integer;
   end record;

   package R is new Hybrid.Domain.Model.Result (Err_Type => String, Ok_Type => User);
   use R;

   function Non_Empty (U : User) return Boolean is (U.Name'Length > 0);
   function Adult     (U : User) return Boolean is (U.Age >= 18);

   function Ensure_Non_Empty is new Ensure (Pred => Non_Empty);
   function Ensure_Adult     is new Ensure (Pred => Adult);

   function Validate (U : User) return Result is
      R1 : Result := Ok (U);
   begin
      R1 := Ensure_Non_Empty (R1, On_Fail => "name empty");
      R1 := Ensure_Adult     (R1, On_Fail => "must be 18+");
      return R1;
   end Validate;
begin
   null;
end Demo_Validate;
```

### B) Unifying two strategies (`Either` → `Fold`)

```ada
pragma Ada_2022;

with Hybrid.Domain.Model.Either;

procedure Demo_Strategies is
   package E is new Hybrid.Domain.Model.Either (Left_Type => Float, Right_Type => Integer);
   use E;

   function Strategy_A return Either is (Left  (3.14159)); -- float branch
   function Strategy_B return Either is (Right (42));      -- int branch

   function Show is new E.Fold
     (T        => String,
      On_Left  => (function (F : Float)   return String is ("F=" & Float'Image (F))),
      On_Right => (function (I : Integer) return String is ("I=" & Integer'Image (I))));
begin
   -- choose a strategy dynamically, but consume uniformly
   declare
      use Ada.Calendar;
      Elt : Either := (if Seconds (Clock) mod 2 = 0 then Strategy_A else Strategy_B);
   begin
      -- Show(Elt) yields a String regardless of which path was chosen
      null;
   end;
end Demo_Strategies;
```

### C) Add context to errors (`Result.Context`)

```ada
-- from the Result demo:
-- function With_Context is new Context (Ctx_Type => String, Add => Add_Ctx);
-- R := With_Context (R, "loading profile:user-123");
```

### D) Sequence operations, stop on first error (`Then`)

```ada
-- Result.Then keeps B if A is Ok; otherwise propagates A's error
R_All := Then (Validate_Config, Then (Connect_DB, Run_Migrations));
```

---

## 5) Interop and conversions

Because `Result` in this design is a **facade over `Either`** (Ok = Right, Err = Left), you don’t need explicit conversion in Ada—`Result` simply re‑exports the useful pieces with error‑focused names. If you also use a separate `Either` instantiation for neutral data, keep them distinct so intent stays obvious at call sites.

If you must interact with exception‑heavy code (e.g., a runtime library that raises), **convert once at the boundary** (catch → `Err`) and keep the interior pure with `Result` pipelines.

---

## 6) Testing tips

- Test **both sides**: one unit test for the Ok path, one for the Err path.
- Prefer **pure checks** via `Is_Ok/Is_Err`, `Get_Ok`, `Get_Err`, and small **folds** that return primitives you can assert on.
- For context chains, assert that the final error contains (or structures) the accumulated info you expect.

---

## 7) Pitfalls to avoid

- **Using `Either` for errors**: it works, but hides intent. Prefer `Result`.
- **Throwing exceptions in the middle**: if you adopt `Result`/`Either`, keep exceptions at the edges.
- **Overusing `use` clauses**: be explicit where it helps readability (e.g., `R.Ok`, `R.Err`).

---

## Starter checklist

- Fallible operation? → **Result** (`Ok/Err`, `Ensure`, `Context`, `Or_Else`, `Then`, `Tap_*`).  
- Two data shapes to unify? → **Either** (`Map`, `Map_Left`, `BiMap`, `Fold`, `Tap_*`).  
- Exceptions from legacy API? → Catch once → return **Result**.

---

### Appendix: What these packages provide (at a glance)

- `Hybrid.Domain.Model.Result` re‑exports a `Result` type over `Either` with constructors (`Ok`, `Err`), predicates (`Is_Ok`, `Is_Err`), accessors (`Get_Ok`, `Get_Err`), and error‑centric combinators (`Map_Err`, `Or_Else`, `Context`, `Ensure`, plus taps, sequencing, and hashing/equality when desired).  
- `Hybrid.Domain.Model.Either` provides a neutral `Either` with `Map`, `Map_Left`, `BiMap`, `Bind/And_Then`, `Filter`, `Fold`, `Tap_*`, `Then`, `Swap`, `Or`, `Coalesce`, and utilities.
