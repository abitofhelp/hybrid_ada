pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Error - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Application.Error is

   use Error_Messages;

   --  Equality operator for Application_Error.
   --
   --  For tagged types, this uses the default component-wise comparison.
   --  This explicit declaration is required for generic instantiation
   --  with the Result type.
   overriding
   function "=" (Left, Right : Application_Error) return Boolean is
   begin
      return
        Left.Use_Case = Right.Use_Case
        and then Left.Operation = Right.Operation
        and then Left.Message = Right.Message;
   end "=";

end Hybrid.Application.Error;
