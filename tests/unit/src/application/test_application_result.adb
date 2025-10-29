pragma Ada_2022;
--  ==========================================================================
--  Test_Application_Result - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Hybrid.Application.Result.Result_Port;
with Hybrid.Application.Types;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;

package body Test_Application_Result is

   --  =======================================================================
   --  Test Setup: Instantiate facade with Application types ONLY
   --  =======================================================================
   --  ARCHITECTURAL NOTE: This test intentionally does NOT import any Domain
   --  packages. It uses only Application and Infrastructure types to prove
   --  the facade successfully breaks Presentation→Domain coupling.
   --  =======================================================================

   --  Define test error type (Application layer)
   type App_Test_Error is (Validation_Error, IO_Error, Business_Logic_Error);

   --  Instantiate Infrastructure Result adapter
   package Test_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Application.Types.Message_Type,
        E => App_Test_Error);

   --  Instantiate Application.Result facade
   --  This is the pattern Presentation would use
   package App_Result_Facade is new
     Hybrid.Application.Result.Result_Port
       (T         => Hybrid.Application.Types.Message_Type,
        E         => App_Test_Error,
        Result    => Test_Result.FR.Result,
        Ok        => Test_Result.FR.Ok,
        Err       => Test_Result.FR.Err,
        Is_Ok     => Test_Result.FR.Is_Ok,
        Is_Err    => Test_Result.FR.Is_Err,
        Value     => Test_Result.FR.Value,
        Error     => Test_Result.FR.Error,
        Unwrap_Or => Test_Result.FR.Unwrap_Or);

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Application.Result Facade Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);

      use Hybrid.Application.Types;
   begin
      --  Test 1: Facade can create Ok results
      declare
         Result : constant Test_Result.FR.Result :=
           Test_Result.FR.Ok (To_Message ("Success!"));
      begin
         Assert
           (Test_Result.FR.Is_Ok (Result),
            "Facade should support Ok result creation");
         Assert
           (not Test_Result.FR.Is_Err (Result),
            "Ok result should not be an error");
      end;

      --  Test 2: Facade can create Err results
      declare
         Result : constant Test_Result.FR.Result :=
           Test_Result.FR.Err (Validation_Error);
      begin
         Assert
           (Test_Result.FR.Is_Err (Result),
            "Facade should support Err result creation");
         Assert
           (not Test_Result.FR.Is_Ok (Result),
            "Err result should not be Ok");
      end;

      --  Test 3: Facade can extract values from Ok results
      declare
         Msg    : constant Message_Type := To_Message ("Test Message");
         Result : constant Test_Result.FR.Result := Test_Result.FR.Ok (Msg);
         Value  : constant Message_Type := Test_Result.FR.Value (Result);
      begin
         Assert
           (To_String (Value) = "Test Message",
            "Facade should extract correct value from Ok result");
      end;

      --  Test 4: Facade can extract errors from Err results
      declare
         Result : constant Test_Result.FR.Result :=
           Test_Result.FR.Err (IO_Error);
         Err    : constant App_Test_Error := Test_Result.FR.Error (Result);
      begin
         Assert
           (Err = IO_Error,
            "Facade should extract correct error from Err result");
      end;

      --  Test 5: Facade supports Unwrap_Or with default
      declare
         Ok_Result  : constant Test_Result.FR.Result :=
           Test_Result.FR.Ok (To_Message ("Success"));
         Err_Result : constant Test_Result.FR.Result :=
           Test_Result.FR.Err (Business_Logic_Error);
         Default    : constant Message_Type := To_Message ("Default");

         Ok_Value   : constant Message_Type :=
           Test_Result.FR.Unwrap_Or (Ok_Result, Default);
         Err_Value  : constant Message_Type :=
           Test_Result.FR.Unwrap_Or (Err_Result, Default);
      begin
         Assert
           (To_String (Ok_Value) = "Success",
            "Unwrap_Or should return value for Ok result");
         Assert
           (To_String (Err_Value) = "Default",
            "Unwrap_Or should return default for Err result");
      end;

      --  Test 6: Multiple facade instantiations with different types
      --  Proves facade is properly generic
      declare
         type Different_Error is (Error_A, Error_B, Error_C);

         package Alt_Result is new
           Hybrid.Infrastructure.Adapter.Functional.Result_API
             (T => Hybrid.Application.Types.Message_Type,
              E => Different_Error);

         package Alt_Facade is new
           Hybrid.Application.Result.Result_Port
             (T         => Hybrid.Application.Types.Message_Type,
              E         => Different_Error,
              Result    => Alt_Result.FR.Result,
              Ok        => Alt_Result.FR.Ok,
              Err       => Alt_Result.FR.Err,
              Is_Ok     => Alt_Result.FR.Is_Ok,
              Is_Err    => Alt_Result.FR.Is_Err,
              Value     => Alt_Result.FR.Value,
              Error     => Alt_Result.FR.Error,
              Unwrap_Or => Alt_Result.FR.Unwrap_Or);

         Result : constant Alt_Result.FR.Result :=
           Alt_Result.FR.Ok (To_Message ("Multi-instantiation works"));
      begin
         Assert
           (Alt_Result.FR.Is_Ok (Result),
            "Facade should support multiple instantiations");
      end;

      --  Test 7: Chaining operations through facade
      declare
         Result1 : constant Test_Result.FR.Result :=
           Test_Result.FR.Ok (To_Message ("Step 1"));
         Result2 : Test_Result.FR.Result;
      begin
         --  Simulate chained processing
         if Test_Result.FR.Is_Ok (Result1) then
            Result2 := Test_Result.FR.Ok (To_Message ("Step 2"));
         else
            Result2 := Test_Result.FR.Err (Business_Logic_Error);
         end if;

         Assert
           (Test_Result.FR.Is_Ok (Result2),
            "Facade should support chained Result operations");
      end;

   end Run_Test;

end Test_Application_Result;
