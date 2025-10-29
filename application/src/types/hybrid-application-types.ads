pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Types - Common Application Layer Types
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Common types used across the Application layer, particularly for
--    public API boundaries. Provides bounded strings to avoid unbounded
--    types in interfaces.
--
--  Design Notes:
--    - Message_Type: Bounded string for output messages (max 1024 chars)
--    - Preferred over Unbounded_String for public interfaces
--    - Provides type safety and predictable memory usage
--  ==========================================================================

with Ada.Strings.Bounded;

package Hybrid.Application.Types
   with Preelaborate
is
   --  Bounded string for output messages
   --  Max length: 1024 characters (suitable for console output, logs)
   package Message_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => 1024);

   subtype Message_Type is Message_Strings.Bounded_String;

   --  Convenience functions
   function To_Message (Source : String) return Message_Type;
   function To_String (Source : Message_Type) return String;

end Hybrid.Application.Types;
