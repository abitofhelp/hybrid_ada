pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Adapter.Console_Output - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE in the project root.
--  ==========================================================================

with Ada.Text_IO;

package body Hybrid.Infrastructure.Adapter.Console_Output is

   function Send (Msg : String) return R.Result is
   begin
      Ada.Text_IO.Put_Line (Msg);
      --  Return success with message converted to Message_Type via generic formal
      return R.Ok (To_Message (Msg));
   exception
      when others =>
         return R.Err (IO_Failure);
   end Send;

end Hybrid.Infrastructure.Adapter.Console_Output;
