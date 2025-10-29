pragma Ada_2022;
--  ==========================================================================
--  Test_Domain_Model_Result - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Hybrid.Domain.Model.Result;
with Ada.Strings.Bounded;

package body Test_Domain_Model_Result is

   --  Use bounded strings for definite error type
   package Error_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);
   subtype Error_String is Error_Strings.Bounded_String;

   use type Error_String;

   function To_Err (S : String) return Error_String is
     (Error_Strings.To_Bounded_String (S));

   --  Test Result instantiation with Integer and bounded string error
   package Int_Result is new Hybrid.Domain.Model.Result
     (E => Error_String,
      T => Integer);

   use type Int_Result.Result;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Domain.Model.Result Monad Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Ok constructor and Is_Ok
      declare
         R : constant Int_Result.Result := Int_Result.Ok (42);
      begin
         Assert (Int_Result.Is_Ok (R), "Ok result should satisfy Is_Ok");
         Assert (not Int_Result.Is_Err (R), "Ok result should not satisfy Is_Err");
      end;

      --  Test 2: Err constructor and Is_Err
      declare
         R : constant Int_Result.Result := Int_Result.Err (To_Err ("error message"));
      begin
         Assert (Int_Result.Is_Err (R), "Err result should satisfy Is_Err");
         Assert (not Int_Result.Is_Ok (R), "Err result should not satisfy Is_Ok");
      end;

      --  Test 3: Get_Ok retrieves correct value
      declare
         R : constant Int_Result.Result := Int_Result.Ok (99);
         Value : constant Integer := Int_Result.Get_Ok (R);
      begin
         Assert (Value = 99, "Get_Ok should return the wrapped value");
      end;

      --  Test 4: Get_Err retrieves correct error
      declare
         R : constant Int_Result.Result := Int_Result.Err (To_Err ("test error"));
         Err : constant Error_String := Int_Result.Get_Err (R);
      begin
         Assert (Err = To_Err ("test error"), "Get_Err should return the wrapped error");
      end;

      --  Test 5: Map_Ok transforms Ok value
      declare
         R : constant Int_Result.Result := Int_Result.Ok (5);

         function Double (X : Integer) return Integer is (X * 2);
         function Map_Double is new Int_Result.Map_Ok (Double);

         R2 : constant Int_Result.Result := Map_Double (R);
      begin
         Assert (Int_Result.Is_Ok (R2), "Map_Ok should preserve Ok state");
         Assert (Int_Result.Get_Ok (R2) = 10, "Map_Ok should double the value");
      end;

      --  Test 6: Map_Ok leaves Err unchanged
      declare
         R : constant Int_Result.Result := Int_Result.Err (To_Err ("error"));

         function Double (X : Integer) return Integer is (X * 2);
         function Map_Double is new Int_Result.Map_Ok (Double);

         R2 : constant Int_Result.Result := Map_Double (R);
      begin
         Assert (Int_Result.Is_Err (R2), "Map_Ok should preserve Err state");
         Assert
           (Int_Result.Get_Err (R2) = To_Err ("error"),
            "Map_Ok should preserve error message");
      end;

      --  Test 7: And_Then chains Ok results
      declare
         R : constant Int_Result.Result := Int_Result.Ok (10);

         function Half_If_Even (X : Integer) return Int_Result.Result is
         begin
            if X mod 2 = 0 then
               return Int_Result.Ok (X / 2);
            else
               return Int_Result.Err (To_Err ("not even"));
            end if;
         end Half_If_Even;

         function Chain_Half is new Int_Result.And_Then (Half_If_Even);

         R2 : constant Int_Result.Result := Chain_Half (R);
      begin
         Assert (Int_Result.Is_Ok (R2), "And_Then should chain Ok results");
         Assert (Int_Result.Get_Ok (R2) = 5, "And_Then should apply function");
      end;

      --  Test 8: And_Then propagates errors
      declare
         R : constant Int_Result.Result := Int_Result.Err (To_Err ("initial error"));

         function Half_If_Even (X : Integer) return Int_Result.Result is
         begin
            if X mod 2 = 0 then
               return Int_Result.Ok (X / 2);
            else
               return Int_Result.Err (To_Err ("not even"));
            end if;
         end Half_If_Even;

         function Chain_Half is new Int_Result.And_Then (Half_If_Even);

         R2 : constant Int_Result.Result := Chain_Half (R);
      begin
         Assert (Int_Result.Is_Err (R2), "And_Then should propagate Err");
         Assert
           (Int_Result.Get_Err (R2) = To_Err ("initial error"),
            "And_Then should preserve original error");
      end;

      --  Test 9: Or_Else recovers from error
      declare
         R : constant Int_Result.Result := Int_Result.Err (To_Err ("error"));

         function Recover (E : Error_String) return Int_Result.Result is
            pragma Unreferenced (E);
         begin
            return Int_Result.Ok (0);
         end Recover;

         function Or_Recover is new Int_Result.Or_Else (Recover);

         R2 : constant Int_Result.Result := Or_Recover (R);
      begin
         Assert (Int_Result.Is_Ok (R2), "Or_Else should recover from error");
         Assert
           (Int_Result.Get_Ok (R2) = 0,
            "Or_Else should apply recovery function");
      end;

      --  Test 10: Or_Else leaves Ok unchanged
      declare
         R : constant Int_Result.Result := Int_Result.Ok (42);

         function Recover (E : Error_String) return Int_Result.Result is
            pragma Unreferenced (E);
         begin
            return Int_Result.Ok (0);
         end Recover;

         function Or_Recover is new Int_Result.Or_Else (Recover);

         R2 : constant Int_Result.Result := Or_Recover (R);
      begin
         Assert (Int_Result.Is_Ok (R2), "Or_Else should preserve Ok state");
         Assert
           (Int_Result.Get_Ok (R2) = 42,
            "Or_Else should preserve Ok value");
      end;

      --  Test 11: Equality works correctly
      declare
         R1 : constant Int_Result.Result := Int_Result.Ok (42);
         R2 : constant Int_Result.Result := Int_Result.Ok (42);
         R3 : constant Int_Result.Result := Int_Result.Ok (99);
         E1 : constant Int_Result.Result := Int_Result.Err (To_Err ("error"));
         E2 : constant Int_Result.Result := Int_Result.Err (To_Err ("error"));
      begin
         Assert (R1 = R2, "Equal Ok results should compare equal");
         Assert (R1 /= R3, "Different Ok results should compare unequal");
         Assert (E1 = E2, "Equal Err results should compare equal");
         Assert (R1 /= E1, "Ok and Err should never compare equal");
      end;

   end Run_Test;

end Test_Domain_Model_Result;
