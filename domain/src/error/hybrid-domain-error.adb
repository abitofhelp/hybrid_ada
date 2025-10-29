pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Error - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Domain.Error is

   use Error_Messages;  -- For bounded string equality operator

   --  Equality operator for Domain_Error.
   --
   --  For tagged types, this provides component-wise comparison.
   --  Required for generic instantiation with Result/Either types.
   overriding
   function "=" (Left, Right : Domain_Error) return Boolean is
   begin
      return
        Left.Entity = Right.Entity and then Left.Operation = Right.Operation
        and then Left.Message = Right.Message;
   end "=";

end Hybrid.Domain.Error;
