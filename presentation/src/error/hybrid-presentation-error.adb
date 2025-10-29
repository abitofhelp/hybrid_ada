pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Presentation.Error - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Presentation.Error is

   use Error_Messages;  -- For bounded string equality operator

   --  Equality operator for Presentation_Error.
   --
   --  For tagged types, this provides component-wise comparison.
   --  Required for generic instantiation with Result/Either types.
   overriding
   function "=" (Left, Right : Presentation_Error) return Boolean is
   begin
      return
        Left.Interface_Type = Right.Interface_Type
        and then Left.Operation = Right.Operation
        and then Left.Message = Right.Message;
   end "=";

end Hybrid.Presentation.Error;
