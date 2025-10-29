pragma Ada_2022;
--  ==========================================================================
--  Test_Application_Error - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Hybrid.Application.Error;

package body Test_Application_Error is

   package Error renames Hybrid.Application.Error;

   use type Error.Error_Message;
   use type Error.Application_Error;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Application.Error Types Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: New_Application_Error constructor
      declare
         Err : constant Error.Application_Error :=
           Error.New_Application_Error
             (Use_Case  => "TestUseCase",
              Operation => "TestOp",
              Message   => "Test message");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Use_Case) = "TestUseCase",
            "Application_Error should store use_case");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "TestOp",
            "Application_Error should store operation");
         Assert
           (Error.Error_Messages.To_String (Err.Message) = "Test message",
            "Application_Error should store message");
      end;

      --  Test 2: Validation_Error convenience constructor
      declare
         Err : constant Error.Application_Error :=
           Error.Validation_Error
             (Use_Case => "CreateUser",
              Field    => "email",
              Reason   => "invalid format");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Use_Case) = "CreateUser",
            "Validation_Error should store use_case");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "Validate_email",
            "Validation_Error should construct operation from field");
         Assert
           (Error.Error_Messages.To_String (Err.Message) =
            "Validation failed for email: invalid format",
            "Validation_Error should have correct message");
      end;

      --  Test 3: Output_Error convenience constructor
      declare
         Err : constant Error.Application_Error :=
           Error.Output_Error
             (Use_Case    => "GenerateReport",
              Output_Type => "PDF",
              Reason      => "disk full");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Use_Case) = "GenerateReport",
            "Output_Error should store use_case");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "Send_Output",
            "Output_Error should use Send_Output operation");
         Assert
           (Error.Error_Messages.To_String (Err.Message) =
            "Output to PDF failed: disk full",
            "Output_Error should have correct message");
      end;

      --  Test 4: Service_Error convenience constructor
      declare
         Err : constant Error.Application_Error :=
           Error.Service_Error
             (Use_Case => "ProcessOrder",
              Service  => "PaymentGateway",
              Reason   => "timeout");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Use_Case) = "ProcessOrder",
            "Service_Error should store use_case");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "Call_PaymentGateway",
            "Service_Error should construct operation from service name");
         Assert
           (Error.Error_Messages.To_String (Err.Message) =
            "Service PaymentGateway failed: timeout",
            "Service_Error should have correct message");
      end;

      --  Test 5: Equality operator
      declare
         E1 : constant Error.Application_Error :=
           Error.Validation_Error ("Test", "field", "reason");
         E2 : constant Error.Application_Error :=
           Error.Validation_Error ("Test", "field", "reason");
         E3 : constant Error.Application_Error :=
           Error.Validation_Error ("Different", "field", "reason");
      begin
         Assert
           (E1 = E2,
            "Two identical Application_Error instances should be equal");
         Assert
           (not (E1 = E3),
            "Different Application_Error instances should not be equal");
      end;
   end Run_Test;

end Test_Application_Error;
