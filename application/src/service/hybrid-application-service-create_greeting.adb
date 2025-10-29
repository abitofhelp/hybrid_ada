pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Service.Create_Greeting - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Notes:
--    - Ada 2022; aspects preferred over pragmas when available.
--    - 78-character separators enforced.
--  ==========================================================================

package body Hybrid.Application.Service.Create_Greeting is
   package body API is
      function Run (Raw_Name : String) return R.Result is
         Res_Name : constant R_Name.Result := Name_Ops.Create (Raw_Name);
      begin
         if R_Name.Is_Err (Res_Name) then
            return R.Err (Map_Error (R_Name.Error (Res_Name)));
         else
            declare
               PN_Str : constant String :=
                 Hybrid.Domain.Value.Person_Name.To_String
                   (R_Name.Value (Res_Name));
               Greet  : constant String := "Hello, " & PN_Str & "!";
            begin
               return R.Ok (To_Message (Greet));
            end;
         end if;
      end Run;
   end API;
end Hybrid.Application.Service.Create_Greeting;
