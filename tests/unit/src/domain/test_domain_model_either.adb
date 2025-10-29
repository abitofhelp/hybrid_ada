pragma Ada_2022;
--  ==========================================================================
--  Test_Domain_Model_Either - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Hybrid.Domain.Model.Either;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Bounded;

package body Test_Domain_Model_Either is

   --  Use bounded strings for definite error type
   package Error_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);
   subtype Error_String is Error_Strings.Bounded_String;

   use type Error_String;

   function To_Err (S : String) return Error_String is
     (Error_Strings.To_Bounded_String (S));

   --  Test Either instantiation with bounded string error and Integer value
   package Int_Either is new Hybrid.Domain.Model.Either
     (Error_Type => Error_String,
      Value_Type => Integer);

   use type Int_Either.Either;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Domain.Model.Either Monad Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Value constructor creates discriminated record with Is_Value = True
      declare
         E : constant Int_Either.Either := Int_Either.Value (42);
      begin
         Assert (E.Is_Value, "Value constructor should set Is_Value to True");
         Assert (Int_Either.Get_Value (E) = 42, "Value should be accessible");
      end;

      --  Test 2: Error constructor creates discriminated record with Is_Value = False
      declare
         E : constant Int_Either.Either := Int_Either.Error (To_Err ("error message"));
      begin
         Assert
           (not E.Is_Value,
            "Error constructor should set Is_Value to False");
         Assert
           (Int_Either.Get_Error (E) = To_Err ("error message"),
            "Error should be accessible");
      end;

      --  Test 3: Map transforms Value
      declare
         E : constant Int_Either.Either := Int_Either.Value (5);

         function Double (X : Integer) return Integer is (X * 2);
         function Map_Double is new Int_Either.Map (Double);

         E2 : constant Int_Either.Either := Map_Double (E);
      begin
         Assert (E2.Is_Value, "Map should preserve Value state");
         Assert (Int_Either.Get_Value (E2) = 10, "Map should double the value");
      end;

      --  Test 4: Map leaves Error unchanged
      declare
         E : constant Int_Either.Either := Int_Either.Error (To_Err ("error"));

         function Double (X : Integer) return Integer is (X * 2);
         function Map_Double is new Int_Either.Map (Double);

         E2 : constant Int_Either.Either := Map_Double (E);
      begin
         Assert (not E2.Is_Value, "Map should preserve Error state");
         Assert
           (Int_Either.Get_Error (E2) = To_Err ("error"),
            "Map should preserve error message");
      end;

      --  Test 5: Bind chains Value results
      declare
         E : constant Int_Either.Either := Int_Either.Value (10);

         function Half_If_Even (X : Integer) return Int_Either.Either is
         begin
            if X mod 2 = 0 then
               return Int_Either.Value (X / 2);
            else
               return Int_Either.Error (To_Err ("not even"));
            end if;
         end Half_If_Even;

         function Bind_Half is new Int_Either.Bind (Half_If_Even);

         E2 : constant Int_Either.Either := Bind_Half (E);
      begin
         Assert (E2.Is_Value, "Bind should chain Value results");
         Assert (Int_Either.Get_Value (E2) = 5, "Bind should apply function");
      end;

      --  Test 6: Bind propagates errors
      declare
         E : constant Int_Either.Either := Int_Either.Error (To_Err ("initial error"));

         function Half_If_Even (X : Integer) return Int_Either.Either is
         begin
            if X mod 2 = 0 then
               return Int_Either.Value (X / 2);
            else
               return Int_Either.Error (To_Err ("not even"));
            end if;
         end Half_If_Even;

         function Bind_Half is new Int_Either.Bind (Half_If_Even);

         E2 : constant Int_Either.Either := Bind_Half (E);
      begin
         Assert (not E2.Is_Value, "Bind should propagate Error");
         Assert
           (Int_Either.Get_Error (E2) = To_Err ("initial error"),
            "Bind should preserve original error");
      end;

      --  Test 7: Or_Else recovers from error
      declare
         E : constant Int_Either.Either := Int_Either.Error (To_Err ("error"));

         function Recover (Err : Error_String) return Int_Either.Either is
            pragma Unreferenced (Err);
         begin
            return Int_Either.Value (0);
         end Recover;

         function Or_Recover is new Int_Either.Or_Else (Recover);

         E2 : constant Int_Either.Either := Or_Recover (E);
      begin
         Assert (E2.Is_Value, "Or_Else should recover from error");
         Assert
           (Int_Either.Get_Value (E2) = 0,
            "Or_Else should apply recovery function");
      end;

      --  Test 8: Or_Else leaves Value unchanged
      declare
         E : constant Int_Either.Either := Int_Either.Value (42);

         function Recover (Err : Error_String) return Int_Either.Either is
            pragma Unreferenced (Err);
         begin
            return Int_Either.Value (0);
         end Recover;

         function Or_Recover is new Int_Either.Or_Else (Recover);

         E2 : constant Int_Either.Either := Or_Recover (E);
      begin
         Assert (E2.Is_Value, "Or_Else should preserve Value state");
         Assert
           (Int_Either.Get_Value (E2) = 42,
            "Or_Else should preserve Value");
      end;

      --  Test 9: Get_Value_Or returns value when Is_Value = True
      declare
         E : constant Int_Either.Either := Int_Either.Value (42);
         V : constant Integer := Int_Either.Get_Value_Or (E, 0);
      begin
         Assert
           (V = 42,
            "Get_Value_Or should return value when Is_Value = True");
      end;

      --  Test 10: Get_Value_Or returns default when Is_Value = False
      declare
         E : constant Int_Either.Either := Int_Either.Error (To_Err ("error"));
         V : constant Integer := Int_Either.Get_Value_Or (E, -1);
      begin
         Assert
           (V = -1,
            "Get_Value_Or should return default when Is_Value = False");
      end;

      --  Test 11: Equality works correctly
      declare
         V1 : constant Int_Either.Either := Int_Either.Value (42);
         V2 : constant Int_Either.Either := Int_Either.Value (42);
         V3 : constant Int_Either.Either := Int_Either.Value (99);
         E1 : constant Int_Either.Either := Int_Either.Error (To_Err ("error"));
         E2 : constant Int_Either.Either := Int_Either.Error (To_Err ("error"));
      begin
         Assert (V1 = V2, "Equal Value results should compare equal");
         Assert (V1 /= V3, "Different Value results should compare unequal");
         Assert (E1 = E2, "Equal Error results should compare equal");
         Assert (V1 /= E1, "Value and Error should never compare equal");
      end;

      --  Test 12: Hash function works
      declare
         V1 : constant Int_Either.Either := Int_Either.Value (42);
         V2 : constant Int_Either.Either := Int_Either.Value (42);

         function Hash_Value (I : Integer) return Hash_Type is
           (Hash_Type (I));

         function Hash_Error (S : Error_String) return Hash_Type is
           (Hash_Type (Error_Strings.Length (S)));

         function Hash is new Int_Either.Hash (Hash_Error, Hash_Value);

         H1 : constant Hash_Type := Hash (V1);
         H2 : constant Hash_Type := Hash (V2);
      begin
         Assert
           (H1 = H2,
            "Equal Either values should have equal hashes");
      end;

   end Run_Test;

end Test_Domain_Model_Either;
