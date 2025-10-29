pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Service.Create_Greeting - Service (port-generic)
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
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Application.Port.Output;

package Hybrid.Application.Service.Create_Greeting is
   generic
      type App_Error is private;
      type Person_Name_Error is private;
      type Message_Type is private;

      with function Map_Error (E : Person_Name_Error) return App_Error is <>;
      with function To_Message (S : String) return Message_Type is <>;

      with package R is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T      => Message_Type,
           E      => App_Error,
           others => <>);

      with package R_Name is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T      => Hybrid.Domain.Value.Person_Name.Person_Name,
           E      => Person_Name_Error,
           others => <>);

      with package Name_Ops is new
        Hybrid.Domain.Value.Person_Name.Operations.API (R => R_Name);

      with package Output_Port is new
        Hybrid.Application.Port.Output.API
          (App_Error    => App_Error,
           Message_Type => Message_Type,
           R            => R,
           others       => <>);

   package API
   is
      --  Output_Port is a formal parameter used by clients when instantiating
      pragma Unreferenced (Output_Port);

      function Run (Raw_Name : String) return R.Result
      with Inline;
   end API;
end Hybrid.Application.Service.Create_Greeting;
