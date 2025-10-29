pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Service.Greeting - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Domain.Service.Greeting is

   overriding
   function Create_Greeting
     (Self : Default_Greeting_Service; Name : Person_Name.Person_Name)
      return String
   is
      pragma Unreferenced (Self);
   begin
      return "Hello, " & Person_Name.To_String (Name) & "!";
   end Create_Greeting;

end Hybrid.Domain.Service.Greeting;
