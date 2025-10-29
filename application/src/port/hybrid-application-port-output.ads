pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Port.Output - Signature (port)
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Notes:
--    - Ada 2022; aspects preferred over pragmas when available.
--    - 78-character separators enforced.
--  ==========================================================================

with Hybrid.Domain.Foundation.Ports.Result_Port;

package Hybrid.Application.Port.Output is
   generic
      type App_Error is private;
      type Message_Type is private;
      with package R is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T      => Message_Type,
           E      => App_Error,
           others => <>);
      pragma Warnings (Off, "* is not referenced");
      with function Send (Msg : Message_Type) return R.Result is <>;
      pragma Warnings (On, "* is not referenced");
   package API is
      --  Formal parameter Send is used by clients when instantiating this generic.
      --  It appears unreferenced in this spec but is referenced at instantiation.
   end API;
end Hybrid.Application.Port.Output;
