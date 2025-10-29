pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Service.Greeting
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Domain service for greeting message generation. Encapsulates business
--    logic for creating personalized greetings from validated person names.
--
--  Usage:
--    Generate greeting from person name:
--       Message := Greeting.Generate (Person_Name);
--       Ada.Text_IO.Put_Line (Message);
--
--  Design Notes:
--    Part of Domain layer - no dependencies on outer layers
--    Uses Ada 2022 contracts (Pre/Post) for design-by-contract
--
--  See Also:
--    Hybrid.Domain.Value.Person_Name - dependency
--    Hybrid.Domain.Error - error types
--    Hybrid.Application.Service - application services
--  ==========================================================================

with Hybrid.Domain.Value.Person_Name;

package Hybrid.Domain.Service.Greeting is

   use Hybrid.Domain.Value;

   -- Greeting service interface
   type Greeting_Service is interface;

   function Create_Greeting
     (Self : Greeting_Service; Name : Person_Name.Person_Name) return String
   is abstract
   with Post'Class => Create_Greeting'Result'Length > 0;

   -- Default implementation
   type Default_Greeting_Service is new Greeting_Service with null record;

   overriding
   function Create_Greeting
     (Self : Default_Greeting_Service; Name : Person_Name.Person_Name)
      return String;

end Hybrid.Domain.Service.Greeting;
