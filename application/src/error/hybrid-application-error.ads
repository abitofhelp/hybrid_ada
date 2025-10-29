pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Error
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines error types for the Application layer. Application errors represent
--    use case failures and port/adapter communication issues with rich context
--    for debugging.
--
--  Usage:
--    Error types are used with Result monad for type-safe error handling:
--       Result := Create (Input);
--       if not Is_Ok (Result) then
--          Handle_Error (Get_Err (Result));
--       end if;
--
--  Design Notes:
--    Application layer - orchestrates domain logic
--    Generic package - requires instantiation with specific types
--
--  See Also:
--    Hybrid.Domain.Error - domain layer errors
--    Hybrid.Infrastructure.Error - infrastructure layer errors
--  ==========================================================================

with Ada.Strings.Bounded;

package Hybrid.Application.Error is

   -- Error message string type
   package Error_Messages is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 512);
   subtype Error_Message is Error_Messages.Bounded_String;

   -- Application error type
   type Application_Error is tagged record
      Use_Case  : Error_Message;
      Operation : Error_Message;
      Message   : Error_Message;
   end record;

   -- Constructor
   function New_Application_Error
     (Use_Case : String; Operation : String; Message : String)
      return Application_Error
   is (Use_Case  => Error_Messages.To_Bounded_String (Use_Case),
       Operation => Error_Messages.To_Bounded_String (Operation),
       Message   => Error_Messages.To_Bounded_String (Message));

   -- Equality operator (explicit declaration for generic instantiation)
   --
   -- Educational Note: Tagged types have an implicit equality operator, but
   -- when used as a generic actual parameter, the compiler needs an explicit
   -- declaration to match the generic formal parameter. The default
   -- implementation compares all components. We mark this as "overriding"
   -- to indicate we're replacing the implicit operator.
   overriding
   function "=" (Left, Right : Application_Error) return Boolean;

   -- Common error patterns (convenience constructors)

   function Validation_Error
     (Use_Case : String; Field : String; Reason : String)
      return Application_Error
   is (New_Application_Error
         (Use_Case  => Use_Case,
          Operation => "Validate_" & Field,
          Message   => "Validation failed for " & Field & ": " & Reason));

   function Output_Error
     (Use_Case : String; Output_Type : String; Reason : String)
      return Application_Error
   is (New_Application_Error
         (Use_Case  => Use_Case,
          Operation => "Send_Output",
          Message   => "Output to " & Output_Type & " failed: " & Reason));

   function Service_Error
     (Use_Case : String; Service : String; Reason : String)
      return Application_Error
   is (New_Application_Error
         (Use_Case  => Use_Case,
          Operation => "Call_" & Service,
          Message   => "Service " & Service & " failed: " & Reason));

end Hybrid.Application.Error;
