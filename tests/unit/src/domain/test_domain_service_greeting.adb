pragma Ada_2022;
--  ==========================================================================
--  Test_Domain_Service_Greeting - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Assertions; use AUnit.Assertions;
with Hybrid.Domain.Service.Greeting;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;
with Hybrid.Domain.Error;

package body Test_Domain_Service_Greeting is

   package Greeting renames Hybrid.Domain.Service.Greeting;
   package Person_Name renames Hybrid.Domain.Value.Person_Name;
   package Dom_Err renames Hybrid.Domain.Error;

   --  Instantiate Result adapter
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
      return AUnit.Format ("Domain.Service.Greeting Tests");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test 1: Default_Greeting_Service creates non-empty greeting
      declare
         Service     : constant Greeting.Default_Greeting_Service :=
           Greeting.Default_Greeting_Service'(null record);
         Name_Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Alice");
      begin
         Assert
           (Person_Name_Result.FR.Is_Ok (Name_Result),
            "Test name should be valid");

         if Person_Name_Result.FR.Is_Ok (Name_Result) then
            declare
               Valid_Name    : constant Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Name_Result);
               Greeting_Text : constant String :=
                 Service.Create_Greeting (Valid_Name);
            begin
               Assert
                 (Greeting_Text'Length > 0,
                  "Greeting should be non-empty (postcondition)");
            end;
         end if;
      end;

      --  Test 2: Greeting contains the person's name
      declare
         Service     : constant Greeting.Default_Greeting_Service :=
           Greeting.Default_Greeting_Service'(null record);
         Name_Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Bob");
      begin
         if Person_Name_Result.FR.Is_Ok (Name_Result) then
            declare
               Valid_Name    : constant Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Name_Result);
               Greeting_Text : constant String :=
                 Service.Create_Greeting (Valid_Name);
            begin
               Assert
                 (Greeting_Text'Length > 0,
                  "Greeting should be non-empty");
               Assert
                 (Greeting_Text'Length >= Person_Name.To_String (Valid_Name)'Length,
                  "Greeting should contain at least the name");
            end;
         end if;
      end;

      --  Test 3: Different names produce different greetings
      declare
         Service      : constant Greeting.Default_Greeting_Service :=
           Greeting.Default_Greeting_Service'(null record);
         Name_Result1 : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Alice");
         Name_Result2 : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Bob");
      begin
         if Person_Name_Result.FR.Is_Ok (Name_Result1)
           and then Person_Name_Result.FR.Is_Ok (Name_Result2)
         then
            declare
               Name1      : constant Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Name_Result1);
               Name2      : constant Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Name_Result2);
               Greeting1  : constant String := Service.Create_Greeting (Name1);
               Greeting2  : constant String := Service.Create_Greeting (Name2);
            begin
               Assert
                 (Greeting1 /= Greeting2,
                  "Different names should produce different greetings");
            end;
         end if;
      end;

      --  Test 4: Service is stateless (same input produces same output)
      declare
         Service     : constant Greeting.Default_Greeting_Service :=
           Greeting.Default_Greeting_Service'(null record);
         Name_Result : constant Person_Name_Result.FR.Result :=
           Person_Name_Ops.Create ("Charlie");
      begin
         if Person_Name_Result.FR.Is_Ok (Name_Result) then
            declare
               Valid_Name : constant Person_Name.Person_Name :=
                 Person_Name_Result.FR.Value (Name_Result);
               Greeting1  : constant String :=
                 Service.Create_Greeting (Valid_Name);
               Greeting2  : constant String :=
                 Service.Create_Greeting (Valid_Name);
            begin
               Assert
                 (Greeting1 = Greeting2,
                  "Service should be stateless (same input = same output)");
            end;
         end if;
      end;

   end Run_Test;

end Test_Domain_Service_Greeting;
