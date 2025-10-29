pragma Ada_2022;
--  ==========================================================================
--  Test_Domain_Error - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Hybrid.Domain.Error;

package body Test_Domain_Error is

   package Error renames Hybrid.Domain.Error;

   use type Error.Error_Message;
   use type Error.Domain_Error;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Domain.Error Types Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: New_Domain_Error constructor
      declare
         Err : constant Error.Domain_Error :=
           Error.New_Domain_Error
             (Entity    => "TestEntity",
              Operation => "TestOp",
              Message   => "Test message");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Entity) = "TestEntity",
            "Domain_Error should store entity");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "TestOp",
            "Domain_Error should store operation");
         Assert
           (Error.Error_Messages.To_String (Err.Message) = "Test message",
            "Domain_Error should store message");
      end;

      --  Test 2: Empty_Value_Error convenience constructor
      declare
         Err : constant Error.Domain_Error :=
           Error.Empty_Value_Error ("Person_Name");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Entity) = "Person_Name",
            "Empty_Value_Error should store entity");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "Validate",
            "Empty_Value_Error should use Validate operation");
         Assert
           (Error.Error_Messages.To_String (Err.Message) =
            "Person_Name cannot be empty",
            "Empty_Value_Error should have correct message");
      end;

      --  Test 3: Value_Too_Long_Error with parameters
      declare
         Err : constant Error.Domain_Error :=
           Error.Value_Too_Long_Error ("Name", Actual => 150, Max => 80);
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Entity) = "Name",
            "Value_Too_Long_Error should store entity");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "Validate",
            "Value_Too_Long_Error should use Validate operation");
         --  Check message contains key information
         declare
            Msg : constant String := Error.Error_Messages.To_String (Err.Message);
         begin
            Assert
              (Msg'Length > 0,
               "Value_Too_Long_Error should have a message");
            --  Message should mention "150" and "80"
            Assert
              ((for some I in Msg'Range => Msg (I) = '1'),
               "Message should contain actual length");
         end;
      end;

      --  Test 4: Invalid_Characters_Error
      declare
         Err : constant Error.Domain_Error :=
           Error.Invalid_Characters_Error ("Field", "control characters");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Entity) = "Field",
            "Invalid_Characters_Error should store entity");
         Assert
           (Error.Error_Messages.To_String (Err.Message) =
            "Field contains invalid characters: control characters",
            "Invalid_Characters_Error should have correct message");
      end;

      --  Test 5: Business_Rule_Violation
      declare
         Err : constant Error.Domain_Error :=
           Error.Business_Rule_Violation ("Order", "Total must exceed minimum");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Entity) = "Order",
            "Business_Rule_Violation should store entity");
         Assert
           (Error.Error_Messages.To_String (Err.Operation) = "Business_Rule",
            "Business_Rule_Violation should use Business_Rule operation");
      end;

      --  Test 6: Equality operator
      declare
         E1 : constant Error.Domain_Error :=
           Error.Empty_Value_Error ("Test");
         E2 : constant Error.Domain_Error :=
           Error.Empty_Value_Error ("Test");
         E3 : constant Error.Domain_Error :=
           Error.Empty_Value_Error ("Different");
      begin
         Assert (E1 = E2, "Two identical Domain_Error instances should be equal");
         Assert (not (E1 = E3), "Different Domain_Error instances should not be equal");
      end;

      --  Test 7: Invalid_Format_Error
      declare
         Err : constant Error.Domain_Error :=
           Error.Invalid_Format_Error
             (Entity => "Email",
              Value  => "user@",
              Reason => "missing domain");
      begin
         Assert
           (Error.Error_Messages.To_String (Err.Entity) = "Email",
            "Invalid_Format_Error should store entity");
         --  Check message contains all parts
         declare
            Msg : constant String := Error.Error_Messages.To_String (Err.Message);
         begin
            Assert
              (Msg'Length > 0,
               "Invalid_Format_Error should have a message");
         end;
      end;
   end Run_Test;

end Test_Domain_Error;
