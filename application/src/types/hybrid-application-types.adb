pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Types - Common Application Layer Types (Body)
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Application.Types is

   function To_Message (Source : String) return Message_Type is
   begin
      return Message_Strings.To_Bounded_String (Source);
   end To_Message;

   function To_String (Source : Message_Type) return String is
   begin
      return Message_Strings.To_String (Source);
   end To_String;

end Hybrid.Application.Types;
