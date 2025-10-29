pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Value.Person_Name - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Notes:
--    - Ada 2022; aspects preferred over pragmas when available.
--    - 78-character separators enforced.
--  ==========================================================================

package body Hybrid.Domain.Value.Person_Name is

   function To_String (V : Person_Name) return String is
   begin
      return V.Data (1 .. V.Len);
   end To_String;

   function Length (V : Person_Name) return Natural is
   begin
      return V.Len;
   end Length;

   function Is_Valid (Raw : String) return Boolean is
   begin
      if Raw'Length = 0 or else Raw'Length > Max_Length then
         return False;
      end if;
      for C of Raw loop
         if Character'Pos (C) < 32 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid;
end Hybrid.Domain.Value.Person_Name;
