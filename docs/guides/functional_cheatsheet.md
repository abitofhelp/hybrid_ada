
# Ada 2022 Functional Error-Handling — One‑Page Cheat Sheet

**Version:** 1.0.0
**Date:** October 28, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released


**Core goal:** catch exceptions *locally* and return data types (`Result`, `Option`, `Either`) — no raises escaping your function.

## 0) Setup (types & instantiations)

```ada
pragma Ada_2022;
with Hybrid.Domain.Model.Result;  use Hybrid.Domain.Model.Result;
with Hybrid.Domain.Model.Option;  use Hybrid.Domain.Model.Option;
with Hybrid.Domain.Model.Try_To_Result;
with Hybrid.Domain.Model.Catch_Only;
with Ada.Exceptions;

type Error_Kind is (IO_Error, Parse_Error, Validation_Error, Timeout, Unexpected);
type Error is record Kind : Error_Kind; Message : String; end record;

package Str_Result is new Hybrid.Domain.Model.Result.Instance (T => String,  E => Error);
package Int_Result is new Hybrid.Domain.Model.Result.Instance (T => Integer, E => Error);
package Str_Option is new Hybrid.Domain.Model.Option.Instance (T => String);
package Int_Option is new Hybrid.Domain.Model.Option.Instance (T => Integer);
```

---

## 1) Catch locally → `Result`

```ada
function Map_Ex (Ex : Ada.Exceptions.Exception_Occurrence) return Error is
  ((Unexpected, Ada.Exceptions.Exception_Information (Ex)));
function Read_File return String; -- may raise

package Read_Try is new Hybrid.Domain.Model.Try_To_Result.Instance
  (T => String, E => Error, T_Result => Str_Result,
   Action => Read_File, Map_Exception => Map_Ex);

R : constant Str_Result.Result := Read_Try.Run;
```

**Why:** callers only see `Ok/Err`, never raises.

---

## 2) Selective catch (`Catch_Only`)

```ada
function Is_IO (Ex : Ada.Exceptions.Exception_Occurrence) return Boolean is (True); -- your test
function Map_IO (Ex : Ada.Exceptions.Exception_Occurrence) return Error is ((IO_Error,
  Ada.Exceptions.Exception_Information (Ex)));
function Read_IO return String; -- may raise IO or others

package Read_IO_Try is new Hybrid.Domain.Model.Catch_Only.Instance
  (T => String, E => Error, T_Result => Str_Result,
   Action => Read_IO, Is_Handled => Is_IO, Map_Exception => Map_IO);

R : Str_Result.Result := Read_IO_Try.Run; -- others re-raise
```

---

## 3) Add context

```ada
function Add_Msg (E : Error; Msg : String) return Error is ((E.Kind, E.Message & " :: " & Msg));
function With_File is new Str_Result.With_Context (Append => Add_Msg);

R := With_File (R, "reading config.yaml");
```

---

## 4) Fallbacks (instead of “orElse”)

```ada
Use : Str_Result.Result := Str_Result.Fallback (Primary, Secondary); -- eager

function Make_Default return Str_Result.Result is (Str_Result.Ok ("computed"));
function Fallback_Make is new Str_Result.Fallback_With (F => Make_Default);
Use := Fallback_Make (Primary); -- lazy on Err
```

*(Compat aliases available: `Hybrid.Domain.Model.Result.Compat.Instance` → `Or_Result`, `Or_Else`.)*

---

## 5) Unwrap with defaults

```ada
X  : Integer := Int_Option.Unwrap_Or (Find (Id), 0);
function Zero return Integer is (0);
function Get_With is new Int_Option.Unwrap_Or_With (F => Zero);
X2 : Integer := Get_With (Find (Id));

Y  : Integer := Int_Result.Unwrap_Or (Parse (S), 0);
function Def return Integer is (0);
function Y_With is new Int_Result.Unwrap_Or_With (F => Def);
Y2 : Integer := Y_With (Parse (S));
```

*(Compat: `Hybrid.Domain.Model.Option.Compat.Instance` → `Get_Or_Else`, `Get_Or_Else_With`.)*

---

## 6) Map vs And_Then

```ada
function Parse_Int (S : String) return Integer is (Integer'Value (S));
function Map_Str_To_Int is new Str_Result.Map
  (U => Integer, U_Result => Int_Result, F => Parse_Int); -- Ok T→Ok U

function Parse_Int_Res (S : String) return Int_Result.Result is
begin return Int_Result.Ok (Integer'Value (S));
exception when others => return Int_Result.Err ((Parse_Error, "bad int")); end;
function Bind_Str_To_Int is new Str_Result.And_Then (F => Parse_Int_Res); -- T→Result U
```

---

## 7) Ensure invariants

```ada
function Non_Zero (X : Integer) return Boolean is (X /= 0);
function To_Err (X : Integer) return Error is ((Validation_Error, "must be non-zero"));
function Ensure_NZ is new Int_Result.Ensure (Pred => Non_Zero, To_Error => To_Err);

R : Int_Result.Result := Ensure_NZ (Parse_Int_Res ("0")); -- Err if 0
```

---

## 8) Recover

```ada
function Default_On (E : Error) return Integer is (0);
function Recover_Int is new Int_Result.Recover (Handle => Default_On);
V : Integer := Recover_Int (Parse_Int_Res ("oops"));

function Fix (E : Error) return Int_Result.Result is (Int_Result.Ok (1));
function Recover_R is new Int_Result.Recover_With (Handle => Fix);
R2 : Int_Result.Result := Recover_R (Parse_Int_Res ("oops"));
```

---

## 9) Batch ops

```ada
subtype Ix is Positive range 1 .. 3;
type Int_Array is array (Ix range <>) of Integer;
type Int_Res_Array is array (Ix range <>) of Int_Result.Result;
package Arr_Result is new Int_Result.Instance (T => Int_Array, E => Error);

A : Int_Res_Array := (Int_Result.Ok (1), Int_Result.Err ((Parse_Error,"x")), Int_Result.Ok (3));
Seq : Arr_Result.Result := Int_Result.Sequence
  (Ix => Ix, T_Array => Int_Array, Result_Array => Int_Res_Array, TA_Result => Arr_Result) (A);
```

---

## 10) Option ↔ Result

```ada
function Missing return Error is ((Validation_Error, "missing"));
function Opt_To_Res is new Int_Option.To_Result (E => Error, T_Result => Int_Result);
R : Int_Result.Result := Opt_To_Res (Find (Id), Missing);
```

---

## 11) Try → map → Result (fused)

```ada
function Action return String; -- may raise
function Map_Ex (Ex : Ada.Exceptions.Exception_Occurrence) return Error is
  ((Unexpected, Ada.Exceptions.Exception_Information (Ex)));
function Len (S : String) return Integer is (S'Length);

with Hybrid.Domain.Model.Try_Map;
package Try_Len is new Hybrid.Domain.Model.Try_Map.Instance
  (T => String, U => Integer, E => Error, U_Result => Int_Result,
   Action => Action, Map_Exception => Map_Ex, F => Len);

R : Int_Result.Result := Try_Len.Run;
```

---

### Quick mapping for “orElse” searchers
- `Result` — **Fallback**, **Fallback_With** (compat: `Or_Result`, `Or_Else`)
- `Option` — **Unwrap_Or**, **Unwrap_Or_With** (compat: `Get_Or_Else`, `Get_Or_Else_With`)

**Tips:** guard extractors with `Pre`; prefer `And_Then` for fallible steps; use `With_Context` for breadcrumbs; keep exceptions at the edges with `Try_*`.
