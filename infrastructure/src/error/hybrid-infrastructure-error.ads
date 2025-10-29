pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Error
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines error types for the Infrastructure layer. Infrastructure errors
--    represent technical failures in adapters, I/O operations, and system
--    resources with rich context for debugging.
--
--  Usage:
--    Error types are used with Result monad for type-safe error handling:
--       Result := Send (Output);
--       if not Is_Ok (Result) then
--          Err := Get_Err (Result);
--          Log (To_String (Err.Component) & ": " & To_String (Err.Message));
--       end if;
--
--  Design Notes:
--    - Infrastructure layer - implements technical capabilities
--    - Tagged record with Component/Operation/Message context
--    - Works with Functional.Result for type-safe error propagation
--
--  See Also:
--    Hybrid.Domain.Error - domain layer errors
--    Hybrid.Application.Error - application layer errors
--  ==========================================================================

with Ada.Strings.Bounded;

package Hybrid.Infrastructure.Error is

   -- Error message string type
   package Error_Messages is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 512);
   subtype Error_Message is Error_Messages.Bounded_String;

   -- Infrastructure error with rich context
   type Infrastructure_Error is tagged record
      Component : Error_Message;  -- Infrastructure component (e.g., "Console_Output", "Logger")
      Operation : Error_Message;  -- Technical operation (e.g., "Write", "Send", "Install")
      Message   : Error_Message;  -- Specific error details
   end record;

   -- Equality operator (explicit declaration for generic instantiation)
   --
   -- Educational Note: Tagged types have implicit equality, but when used as
   -- a generic actual parameter, the compiler needs an explicit declaration.
   -- This uses component-wise comparison of all fields.
   overriding
   function "=" (Left, Right : Infrastructure_Error) return Boolean;

   -- General constructor
   function New_Infrastructure_Error
     (Component : String; Operation : String; Message : String)
      return Infrastructure_Error
   is (Component => Error_Messages.To_Bounded_String (Component),
       Operation => Error_Messages.To_Bounded_String (Operation),
       Message   => Error_Messages.To_Bounded_String (Message));

   -- Common error patterns (convenience constructors)

   function IO_Error
     (Component : String; Operation : String; Details : String)
      return Infrastructure_Error
   is (New_Infrastructure_Error
         (Component => Component,
          Operation => Operation,
          Message   => "I/O operation failed: " & Details));

   function Device_Error
     (Component : String; Device : String; Reason : String)
      return Infrastructure_Error
   is (New_Infrastructure_Error
         (Component => Component,
          Operation => "Access_Device",
          Message   => "Device '" & Device & "' error: " & Reason));

   function System_Resource_Error
     (Component : String; Resource : String; Reason : String)
      return Infrastructure_Error
   is (New_Infrastructure_Error
         (Component => Component,
          Operation => "Acquire_Resource",
          Message   => "System resource '" & Resource & "' error: " & Reason));

   function Adapter_Error
     (Adapter : String; Operation : String; Details : String)
      return Infrastructure_Error
   is (New_Infrastructure_Error
         (Component => Adapter & "_Adapter",
          Operation => Operation,
          Message   => "Adapter operation failed: " & Details));

end Hybrid.Infrastructure.Error;
