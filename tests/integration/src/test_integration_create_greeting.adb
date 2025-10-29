pragma Ada_2022;
--  ==========================================================================
--  Test_Integration_Create_Greeting - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hybrid.Application.Service.Create_Greeting;
with Hybrid.Application.Types;
with Hybrid.Application.Port.Output;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Infrastructure.Adapter.Console_Output;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;
with Hybrid.Application.Error;
with Hybrid.Domain.Error;

package body Test_Integration_Create_Greeting is

   --  ==========================================================================
   --  Integration Test Setup: Application Layer + Real Infrastructure
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

   --  Console IO error for integration test
   Console_IO_Error : constant App_Err.Application_Error :=
     App_Err.Output_Error
       (Use_Case    => "Integration_Test_Console",
        Output_Type => "stdout",
        Reason      => "Device unavailable or write failed");

   --  Instantiate REAL Console Output adapter (not a mock!)
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
          (Use_Case  => "Integration_CreateGreeting",
           Operation => To_String (E.Operation) & "_" & To_String (E.Entity),
           Message   => To_String (E.Message));
   end Map_Error;

   --  Instantiate Create_Greeting Service
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
      return AUnit.Format ("Integration: Application + Infrastructure");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Application layer integrates with REAL infrastructure adapter
      --  DIFFERENCE from unit tests: Uses Console_Output adapter (not mock)
      --  DIFFERENCE from E2E tests: Does NOT include presentation/CLI layer
      declare
         Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("IntegrationTest");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result),
            "Integration: Service should succeed with real console adapter");
      end;

      --  Test 2: Error handling flows correctly through layers
      --  Domain validation error → Application → Infrastructure
      declare
         Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("");  --  Empty name should fail validation
      begin
         Assert
           (App_Result.FR.Is_Err (Result),
            "Integration: Domain errors should propagate through layers");
      end;

      --  Test 3: Multiple layer interactions work correctly
      --  Application service → Real console adapter
      declare
         Result1 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Alice");
         Result2 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Bob");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result1),
            "Integration: First greeting should succeed");
         Assert
           (App_Result.FR.Is_Ok (Result2),
            "Integration: Second greeting should succeed");
      end;

      --  Test 4: Service is stateless across calls
      declare
         Result1 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Charlie");
         Result2 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Charlie");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result1),
            "Integration: First call should succeed");
         Assert
           (App_Result.FR.Is_Ok (Result2),
            "Integration: Second call with same input should succeed");
      end;

   end Run_Test;

end Test_Integration_Create_Greeting;
