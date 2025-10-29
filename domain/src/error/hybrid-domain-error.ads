pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Error
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines error types for the Domain layer. Domain errors represent
--    business rule violations and domain constraint failures with rich
--    context for debugging.
--
--  Usage:
--    Error types are used with Result monad for type-safe error handling:
--       Result := Validate (Input);
--       if not Is_Ok (Result) then
--          Err := Get_Err (Result);
--          Log (To_String (Err.Entity) & ": " & To_String (Err.Message));
--       end if;
--
--  Design Notes:
--    - Domain layer - no dependencies on outer layers
--    - Tagged record with Entity/Operation/Message context
--    - Works with Functional.Result for type-safe error propagation
--
--  See Also:
--    Hybrid.Application.Error - application layer errors
--    Hybrid.Infrastructure.Error - infrastructure layer errors
--  ==========================================================================

with Ada.Strings.Bounded;

package Hybrid.Domain.Error is

   -- Error message string type
   package Error_Messages is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 512);
   subtype Error_Message is Error_Messages.Bounded_String;

   -- Domain error with rich context
   type Domain_Error is tagged record
      Entity    : Error_Message;  -- Domain entity (e.g., "Person_Name", "Order")
      Operation : Error_Message;  -- Domain operation (e.g., "Validate", "Create")
      Message   : Error_Message;  -- Specific error details
   end record;

   -- Equality operator (explicit declaration for generic instantiation)
   --
   -- Educational Note: Tagged types have implicit equality, but when used as
   -- a generic actual parameter, the compiler needs an explicit declaration.
   -- This uses component-wise comparison of all fields.
   overriding
   function "=" (Left, Right : Domain_Error) return Boolean;

   -- General constructor
   function New_Domain_Error
     (Entity : String; Operation : String; Message : String)
      return Domain_Error
   is (Entity    => Error_Messages.To_Bounded_String (Entity),
       Operation => Error_Messages.To_Bounded_String (Operation),
       Message   => Error_Messages.To_Bounded_String (Message));

   -- Common error patterns (convenience constructors)

   function Empty_Value_Error (Entity : String) return Domain_Error
   is (New_Domain_Error
         (Entity    => Entity,
          Operation => "Validate",
          Message   => Entity & " cannot be empty"));

   function Value_Too_Long_Error
     (Entity : String; Actual : Natural; Max : Natural) return Domain_Error
   is (New_Domain_Error
         (Entity    => Entity,
          Operation => "Validate",
          Message   =>
            Entity & " length " & Actual'Image & " exceeds maximum of" &
            Max'Image & " characters"));

   function Invalid_Format_Error
     (Entity : String; Value : String; Reason : String) return Domain_Error
   is (New_Domain_Error
         (Entity    => Entity,
          Operation => "Validate",
          Message   =>
            Entity & " '" & Value & "' has invalid format: " & Reason));

   function Invalid_Characters_Error
     (Entity : String; Invalid_Chars : String) return Domain_Error
   is (New_Domain_Error
         (Entity    => Entity,
          Operation => "Validate",
          Message   => Entity & " contains invalid characters: " & Invalid_Chars));

   function Business_Rule_Violation
     (Entity : String; Rule : String) return Domain_Error
   is (New_Domain_Error
         (Entity    => Entity,
          Operation => "Business_Rule",
          Message   => "Business rule violated: " & Rule));

end Hybrid.Domain.Error;
