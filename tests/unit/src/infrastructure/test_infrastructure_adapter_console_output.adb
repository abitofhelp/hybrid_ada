pragma Ada_2022;
--  ==========================================================================
--  Test_Infrastructure_Adapter_Console_Output - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hybrid.Application.Types;
with Hybrid.Infrastructure.Adapter.Console_Output;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;

package body Test_Infrastructure_Adapter_Console_Output is

   --  Define test error type
   type Test_Error is (IO_Error, Other_Error);

   --  Instantiate Result adapter
   package Test_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Application.Types.Message_Type,
        E => Test_Error);

   --  Instantiate Console Output adapter
   package Console_Output is new
     Hybrid.Infrastructure.Adapter.Console_Output
       (App_Error    => Test_Error,
        Message_Type => Hybrid.Application.Types.Message_Type,
        R            => Test_Result.Instance,
        To_Message   => Hybrid.Application.Types.To_Message,
        IO_Failure   => IO_Error);

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Infrastructure.Adapter.Console_Output Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Send with simple message succeeds
      declare
         Result : constant Test_Result.FR.Result :=
           Console_Output.Send ("Hello, World!");
      begin
         Assert
           (Test_Result.FR.Is_Ok (Result),
            "Send should succeed with simple message");
      end;

      --  Test 2: Send with empty message succeeds
      declare
         Result : constant Test_Result.FR.Result :=
           Console_Output.Send ("");
      begin
         Assert
           (Test_Result.FR.Is_Ok (Result),
            "Send should succeed with empty message");
      end;

      --  Test 3: Send with long message succeeds
      declare
         Long_Message : constant String (1 .. 1000) := [others => 'A'];
         Result       : constant Test_Result.FR.Result :=
           Console_Output.Send (Long_Message);
      begin
         Assert
           (Test_Result.FR.Is_Ok (Result),
            "Send should succeed with long message");
      end;

      --  Test 4: Send with special characters succeeds
      declare
         Result : constant Test_Result.FR.Result :=
           Console_Output.Send ("Message with special chars: @#$%^&*()");
      begin
         Assert
           (Test_Result.FR.Is_Ok (Result),
            "Send should succeed with special characters");
      end;

      --  Test 5: Multiple sends succeed (adapter is stateless)
      declare
         Result1 : constant Test_Result.FR.Result :=
           Console_Output.Send ("First message");
         Result2 : constant Test_Result.FR.Result :=
           Console_Output.Send ("Second message");
      begin
         Assert
           (Test_Result.FR.Is_Ok (Result1),
            "First send should succeed");
         Assert
           (Test_Result.FR.Is_Ok (Result2),
            "Second send should succeed (adapter is stateless)");
      end;

   end Run_Test;

end Test_Infrastructure_Adapter_Console_Output;
