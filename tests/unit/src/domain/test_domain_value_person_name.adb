pragma Ada_2022;
--  ==========================================================================
--  Test_Domain_Value_Person_Name - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Domain.Error;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;

package body Test_Domain_Value_Person_Name is

   package Person_Name renames Hybrid.Domain.Value.Person_Name;
   package Dom_Err renames Hybrid.Domain.Error;

   --  Instantiate Result adapter for Person_Name with Domain.Error
   package Person_Name_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Person_Name.Person_Name,
        E => Dom_Err.Domain_Error);

   --  Instantiate Operations API
   package Person_Name_Ops is new
     Person_Name.Operations.API
       (R => Person_Name_Result.Instance);

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Domain.Value.PersonName Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Create with valid name succeeds
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Alice");
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Result),
            "Create should succeed with valid name");
      end;

      --  Test 2: Valid name can be converted to string
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Bob");
      begin
         if Person_Name_Result.FR.Is_Ok (Result) then
            declare
               Name_Value : constant Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Result);
               Name_Str   : constant String :=
                 Person_Name.To_String (Name_Value);
            begin
               Assert
                 (Name_Str = "Bob",
                  "To_String should return original value");
            end;
         end if;
      end;

      --  Test 3: Empty name fails validation
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("");
      begin
         Assert
           (Person_Name_Result.FR.Is_Err (Result),
            "Create should fail with empty name");
      end;

      --  Test 4: Whitespace-only name currently succeeds (future: add stricter validation)
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("   ");
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Result),
            "Create currently accepts whitespace-only names");
      end;

      --  Test 5: Name with valid characters succeeds
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Mary-Jane O'Brien");
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Result),
            "Create should succeed with hyphens and apostrophes");
      end;

      --  Test 6: Name at max length succeeds
      declare
         Max_Name : constant String (1 .. Person_Name.Max_Length) :=
           [others => 'A'];
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create (Max_Name);
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Result),
            "Create should succeed with max length name");
      end;

      --  Test 7: Name exceeding max length fails
      declare
         Too_Long : constant String (1 .. Person_Name.Max_Length + 1) :=
           [others => 'A'];
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create (Too_Long);
      begin
         Assert
           (Person_Name_Result.FR.Is_Err (Result),
            "Create should fail with name exceeding max length");
      end;

      --  Test 8: Name with numbers currently succeeds (future: add stricter validation)
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Alice123");
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Result),
            "Create currently accepts numbers in names");
      end;

      --  Test 9: Name with special chars currently succeeds (future: add stricter validation)
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Alice@Smith");
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Result),
            "Create currently accepts special characters");
      end;

      --  Test 10: Length function returns correct value
      declare
         Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Alice");
      begin
         if Person_Name_Result.FR.Is_Ok (Result) then
            declare
               Name_Value : constant Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Result);
            begin
               Assert
                 (Person_Name.Length (Name_Value) = 5,
                  "Length should return 5 for 'Alice'");
            end;
         end if;
      end;

   end Run_Test;

end Test_Domain_Value_Person_Name;
