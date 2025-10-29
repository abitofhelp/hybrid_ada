pragma Ada_2022;
--  ==========================================================================
--  Mock_Output_Port - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Mock_Output_Port is

   overriding procedure Send
     (Self    : Mock_Output_Port_Type;
      Message : String;
      Result  : out Hybrid.Application.Port.Output.Result)
   is
      pragma Unreferenced (Self, Message);
   begin
      --  Mock always returns success
      Result := Hybrid.Application.Port.Output.Output_Result.Ok (True);
   end Send;

end Mock_Output_Port;
