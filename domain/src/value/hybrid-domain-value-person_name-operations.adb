pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Value.Person_Name.Operations - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Notes:
--    - Ada 2022; aspects preferred over pragmas when available.
--    - 78-character separators enforced.
--  ==========================================================================

package body Hybrid.Domain.Value.Person_Name.Operations is
   package PN renames Hybrid.Domain.Value.Person_Name;
   package Err renames Hybrid.Domain.Error;

   package body API is
      function Create (Raw : String) return R.Result is
         Name : PN.Person_Name;
      begin
         if Raw'Length = 0 then
            return R.Err (Err.Empty_Value_Error ("Person_Name"));
         elsif Raw'Length > PN.Max_Length then
            return
              R.Err
                (Err.Value_Too_Long_Error
                   ("Person_Name", Raw'Length, PN.Max_Length));
         else
            for C of Raw loop
               if Character'Pos (C) < 32 then
                  return
                    R.Err
                      (Err.Invalid_Characters_Error
                         ("Person_Name", "control characters"));
               end if;
            end loop;
            Name.Data (1 .. Raw'Length) := Raw;
            Name.Len := Raw'Length;
            return R.Ok (Name);
         end if;
      end Create;
   end API;
end Hybrid.Domain.Value.Person_Name.Operations;
