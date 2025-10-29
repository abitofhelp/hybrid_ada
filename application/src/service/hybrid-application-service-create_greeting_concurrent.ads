pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Service.Create_Greeting_Concurrent
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Concurrent greeting service using Ada tasks for async processing.
--    Demonstrates Ada's built-in concurrency features with port-based architecture.
--
--  Usage:
--    -- Instantiate the concurrent service
--    package Concurrent_Greeting is new
--      Create_Greeting_Concurrent.API
--        (App_Error => My_Error,
--         Person_Name_Error => PN_Error,
--         Map_Error => Map_Fn,
--         R => My_Result,
--         R_Name => PN_Result,
--         Name_Ops => PN_Ops,
--         Output_Port => Out_Port);
--
--    -- Use it
--    Result := Concurrent_Greeting.Run ("Alice");
--
--  Design Notes:
--    - Generic over ports (same signature as Create_Greeting)
--    - Uses tasks for async greeting processing
--    - Thread-safe via protected objects
--    - Demonstrates Ada concurrency features
--  ==========================================================================

with Ada.Strings.Unbounded;
with Hybrid.Domain.Foundation.Ports.Result_Port;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Application.Port.Output;

package Hybrid.Application.Service.Create_Greeting_Concurrent is
   generic
      type App_Error is private;
      type Person_Name_Error is private;

      with function Map_Error (E : Person_Name_Error) return App_Error is <>;

      with package R is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T      => Ada.Strings.Unbounded.Unbounded_String,
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
           Message_Type => Ada.Strings.Unbounded.Unbounded_String,
           R            => R,
           others       => <>);

   package API
   is
      -- Run greeting service (synchronous interface, async implementation)
      function Run (Raw_Name : String) return R.Result;

   end API;
end Hybrid.Application.Service.Create_Greeting_Concurrent;
