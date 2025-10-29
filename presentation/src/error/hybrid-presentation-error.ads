pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Presentation.Error
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines error types for the Presentation layer. Presentation errors
--    represent user interface issues, input parsing failures, and display
--    problems with rich context for debugging.
--
--  Usage:
--    Error types are used with Result monad for type-safe error handling:
--       Result := Parse_Args;
--       if not Is_Ok (Result) then
--          Err := Get_Err (Result);
--          Show_Error (To_String (Err.Message));
--       end if;
--
--  Design Notes:
--    - Presentation layer - user interface concerns
--    - Tagged record with Interface_Type/Operation/Message context
--    - Works with Functional.Result for type-safe error propagation
--
--  See Also:
--    Hybrid.Application.Error - application layer errors
--    Hybrid.Presentation.Exit_Code - exit code mappings
--  ==========================================================================

with Ada.Strings.Bounded;

package Hybrid.Presentation.Error is

   -- Error message string type
   package Error_Messages is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 512);
   subtype Error_Message is Error_Messages.Bounded_String;

   -- Presentation error with rich context
   type Presentation_Error is tagged record
      Interface_Type : Error_Message;  -- Interface type (e.g., "CLI", "GUI", "API")
      Operation      : Error_Message;  -- UI operation (e.g., "Parse_Args", "Display")
      Message        : Error_Message;  -- Specific error details
   end record;

   -- Equality operator (explicit declaration for generic instantiation)
   --
   -- Educational Note: Tagged types have implicit equality, but when used as
   -- a generic actual parameter, the compiler needs an explicit declaration.
   -- This uses component-wise comparison of all fields.
   overriding
   function "=" (Left, Right : Presentation_Error) return Boolean;

   -- General constructor
   function New_Presentation_Error
     (Interface_Type : String; Operation : String; Message : String)
      return Presentation_Error
   is (Interface_Type => Error_Messages.To_Bounded_String (Interface_Type),
       Operation      => Error_Messages.To_Bounded_String (Operation),
       Message        => Error_Messages.To_Bounded_String (Message));

   -- Common error patterns (convenience constructors)

   function Argument_Parse_Error
     (Argument : String; Reason : String) return Presentation_Error
   is (New_Presentation_Error
         (Interface_Type => "CLI",
          Operation      => "Parse_Argument",
          Message        => "Argument '" & Argument & "' parse failed: " & Reason));

   function Missing_Argument_Error (Required : String) return Presentation_Error
   is (New_Presentation_Error
         (Interface_Type => "CLI",
          Operation      => "Validate_Arguments",
          Message        => "Missing required argument: " & Required));

   function Unknown_Flag_Error (Flag : String) return Presentation_Error
   is (New_Presentation_Error
         (Interface_Type => "CLI",
          Operation      => "Parse_Flags",
          Message        => "Unknown flag: " & Flag));

   function Invalid_Flag_Combination_Error
     (Flags : String) return Presentation_Error
   is (New_Presentation_Error
         (Interface_Type => "CLI",
          Operation      => "Validate_Flags",
          Message        => "Invalid flag combination: " & Flags));

   function Display_Error (Details : String) return Presentation_Error
   is (New_Presentation_Error
         (Interface_Type => "CLI",
          Operation      => "Display_Output",
          Message        => "Display failed: " & Details));

end Hybrid.Presentation.Error;
