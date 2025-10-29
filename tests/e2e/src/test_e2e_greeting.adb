pragma Ada_2022;
--  ==========================================================================
--  Test_E2E_Greeting - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Domain.Service.Greeting;
with Hybrid.Application.Service.Create_Greeting;
with Hybrid.Application.Types;
with Hybrid.Application.Port.Output;
with Hybrid.Infrastructure.Adapter.Console_Output;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;
with Hybrid.Application.Error;
with Hybrid.Domain.Error;

package body Test_E2E_Greeting is

   --  ==========================================================================
   --  E2E Test Setup: Complete Stack from Domain to Infrastructure
   --  ==========================================================================
   --  This mirrors the composition root (bootstrap/main.adb) but in a test
   --  context. Tests the complete integration of all architectural layers.
   --  ==========================================================================

   --  Type aliases for error types
   package App_Err renames Hybrid.Application.Error;
   package Dom_Err renames Hybrid.Domain.Error;

   --  Instantiate Result adapters
   package App_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Application.Types.Message_Type,
        E => App_Err.Application_Error);

   package Person_Name_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Domain.Value.Person_Name.Person_Name,
        E => Dom_Err.Domain_Error);

   --  Instantiate Person_Name Operations
   package Person_Name_Ops is new
     Hybrid.Domain.Value.Person_Name.Operations.API
       (R => Person_Name_Result.Instance);

   --  Console IO error for test
   Console_IO_Error : constant App_Err.Application_Error :=
     App_Err.Output_Error
       (Use_Case    => "E2E_Test_Console",
        Output_Type => "stdout",
        Reason      => "Device unavailable or write failed");

   --  Instantiate Console Output adapter
   package Console_Output is new
     Hybrid.Infrastructure.Adapter.Console_Output
       (App_Error    => App_Err.Application_Error,
        Message_Type => Hybrid.Application.Types.Message_Type,
        R            => App_Result.Instance,
        To_Message   => Hybrid.Application.Types.To_Message,
        IO_Failure   => Console_IO_Error);

   --  Wrapper function to adapt Message_Type -> String
   function Send_Message
     (Msg : Hybrid.Application.Types.Message_Type) return App_Result.FR.Result
   is
   begin
      return Console_Output.Send (Hybrid.Application.Types.To_String (Msg));
   end Send_Message;

   --  Instantiate Output Port
   package Output_Port is new
     Hybrid.Application.Port.Output.API
       (App_Error    => App_Err.Application_Error,
        Message_Type => Hybrid.Application.Types.Message_Type,
        R            => App_Result.Instance,
        Send         => Send_Message);

   --  Error mapping function: Domain.Error → Application.Error
   function Map_Error (E : Dom_Err.Domain_Error)
     return App_Err.Application_Error
   is
      use Dom_Err.Error_Messages;
   begin
      return
        App_Err.New_Application_Error
          (Use_Case  => "E2E_CreateGreeting",
           Operation => To_String (E.Operation) & "_" & To_String (E.Entity),
           Message   => To_String (E.Message));
   end Map_Error;

   --  Instantiate Create_Greeting Service (Application Layer)
   package Greeting_Service is new
     Hybrid.Application.Service.Create_Greeting.API
       (App_Error         => App_Err.Application_Error,
        Person_Name_Error => Dom_Err.Domain_Error,
        Message_Type      => Hybrid.Application.Types.Message_Type,
        Map_Error         => Map_Error,
        To_Message        => Hybrid.Application.Types.To_Message,
        R                 => App_Result.Instance,
        R_Name            => Person_Name_Result.Instance,
        Name_Ops          => Person_Name_Ops,
        Output_Port       => Output_Port);

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("E2E Integration Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Full stack with valid input
      --  Domain → Application → Infrastructure
      --  NOTE: This test validates the ENTIRE vertical slice
      declare
         Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Alice");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result),
            "E2E: Valid name should succeed through entire stack");
      end;

      --  Test 2: Domain validation failure propagates correctly
      declare
         Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("");  --  Empty name fails domain validation
      begin
         Assert
           (App_Result.FR.Is_Err (Result),
            "E2E: Domain validation errors should propagate to application layer");
      end;

      --  Test 3: Person_Name value object integration
      declare
         Name_Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Bob");
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Name_Result),
            "E2E: PersonName value object should work correctly");

         if Person_Name_Result.FR.Is_Ok (Name_Result) then
            declare
               Valid_Name : constant Hybrid.Domain.Value.Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Name_Result);
               Domain_Svc : Hybrid.Domain.Service.Greeting.Default_Greeting_Service;
               Greeting_Text : constant String :=
                 Domain_Svc.Create_Greeting (Valid_Name);
            begin
               Assert
                 (Greeting_Text'Length > 0,
                  "E2E: Greeting service should produce non-empty greeting");
               Assert
                 (Greeting_Text'Length >=
                    Hybrid.Domain.Value.Person_Name.To_String (Valid_Name)'Length,
                  "E2E: Greeting should contain the person's name");
            end;
         end if;
      end;

      --  Test 4: Multiple complete flows through all layers
      declare
         Result1 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Charlie");
         Result2 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Diana");
         Result3 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Eve");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result1),
            "E2E: First complete flow should succeed");
         Assert
           (App_Result.FR.Is_Ok (Result2),
            "E2E: Second complete flow should succeed");
         Assert
           (App_Result.FR.Is_Ok (Result3),
            "E2E: Third complete flow should succeed");
      end;

      --  Test 5: Error and success paths can interleave
      declare
         Success1 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Frank");
         Failure1 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("");  --  Empty
         Success2 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Grace");
      begin
         Assert
           (App_Result.FR.Is_Ok (Success1),
            "E2E: Success path should work");
         Assert
           (App_Result.FR.Is_Err (Failure1),
            "E2E: Error path should work");
         Assert
           (App_Result.FR.Is_Ok (Success2),
            "E2E: Error path should not affect subsequent success");
      end;

   end Run_Test;

end Test_E2E_Greeting;
