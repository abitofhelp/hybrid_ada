pragma Ada_2022;
--  ==========================================================================
--  Test_Application_Service_Create_Greeting - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hybrid.Application.Service.Create_Greeting;
with Hybrid.Application.Types;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;
with Hybrid.Application.Port.Output;
with Hybrid.Application.Error;
with Hybrid.Domain.Error;

package body Test_Application_Service_Create_Greeting is

   --  ==========================================================================
   --  Step 1: Type Aliases for Error Types
   --  ==========================================================================

   package App_Err renames Hybrid.Application.Error;
   package Dom_Err renames Hybrid.Domain.Error;

   --  ==========================================================================
   --  Step 2: Instantiate Result Adapters
   --  ==========================================================================

   package App_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Application.Types.Message_Type,
        E => App_Err.Application_Error);

   package Person_Name_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Domain.Value.Person_Name.Person_Name,
        E => Dom_Err.Domain_Error);

   --  ==========================================================================
   --  Step 3: Instantiate Person_Name Operations
   --  ==========================================================================

   package Person_Name_Ops is new
     Hybrid.Domain.Value.Person_Name.Operations.API
       (R => Person_Name_Result.Instance);

   --  ==========================================================================
   --  Step 4: Mock Output Port
   --  ==========================================================================

   function Mock_Send
     (Msg : Hybrid.Application.Types.Message_Type) return App_Result.FR.Result
   is
      pragma Unreferenced (Msg);
   begin
      return App_Result.FR.Ok (Hybrid.Application.Types.To_Message (""));
   end Mock_Send;

   package Mock_Output_Port is new
     Hybrid.Application.Port.Output.API
       (App_Error    => App_Err.Application_Error,
        Message_Type => Hybrid.Application.Types.Message_Type,
        R            => App_Result.Instance,
        Send         => Mock_Send);

   --  ==========================================================================
   --  Step 5: Error Mapping Function
   --  ==========================================================================

   --  Maps Domain.Error → Application.Error at test boundary
   function Map_Error (E : Dom_Err.Domain_Error)
     return App_Err.Application_Error
   is
      use Dom_Err.Error_Messages;
   begin
      return
        App_Err.New_Application_Error
          (Use_Case  => "CreateGreeting_Test",
           Operation => To_String (E.Operation) & "_" & To_String (E.Entity),
           Message   => To_String (E.Message));
   end Map_Error;

   --  ==========================================================================
   --  Step 6: Instantiate Service Under Test
   --  ==========================================================================

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
        Output_Port       => Mock_Output_Port);

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Application.Service.Create_Greeting Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Run with valid name succeeds
      declare
         Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Alice");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result),
            "Run should succeed with valid name 'Alice'");
      end;

      --  Test 2: Run with another valid name succeeds
      declare
         Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Bob");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result),
            "Run should succeed with valid name 'Bob'");
      end;

      --  Test 3: Run with whitespace-only name currently succeeds
      declare
         Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("   ");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result),
            "Run currently accepts whitespace-only names");
      end;

      --  Test 4: Run with too-long name fails validation
      declare
         Long_Name : constant String (1 .. 300) := [others => 'A'];
         Result    : constant App_Result.FR.Result :=
           Greeting_Service.Run (Long_Name);
      begin
         Assert
           (App_Result.FR.Is_Err (Result),
            "Run should fail with name exceeding max length");
      end;

      --  Test 5: Multiple calls work correctly (stateless)
      declare
         Result1 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Alice");
         Result2 : constant App_Result.FR.Result :=
           Greeting_Service.Run ("Bob");
      begin
         Assert
           (App_Result.FR.Is_Ok (Result1),
            "First call should succeed");
         Assert
           (App_Result.FR.Is_Ok (Result2),
            "Second call should succeed (service is stateless)");
      end;

      --  Test 6: Service handles domain errors correctly
      declare
         Empty_Result : constant App_Result.FR.Result :=
           Greeting_Service.Run ("");
      begin
         Assert
           (App_Result.FR.Is_Err (Empty_Result),
            "Service should propagate domain validation errors");
      end;

   end Run_Test;

end Test_Application_Service_Create_Greeting;
