pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Value.Person_Name - Value object (types)
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Notes:
--    - Ada 2022; aspects preferred over pragmas when available.
--    - 78-character separators enforced.
--  ==========================================================================

package Hybrid.Domain.Value.Person_Name is
   Max_Length : constant := 80;

   type Person_Name is private;

   function To_String (V : Person_Name) return String
   with Inline;
   function Is_Valid (Raw : String) return Boolean
   with Inline;
   function Length (V : Person_Name) return Natural
   with Inline;

private
   type Person_Name is record
      Data : String (1 .. Max_Length) := [others => ' '];
      Len  : Natural := 0;
   end record;
end Hybrid.Domain.Value.Person_Name;
