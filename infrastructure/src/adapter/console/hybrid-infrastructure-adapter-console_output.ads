pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Adapter.Console_Output
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE in the project root.
--
--  Purpose:
--    Console output adapter. Provides a Send function that writes to console
--    and can be bound to the Output port signature.
--
--  Usage:
--    -- Define error type
--    type App_Error is (IO_Error, ...);
--
--    -- Instantiate Result adapter
--    package R is new Infrastructure.Adapter.Functional.Result_API
--      (T => String, E => App_Error);
--
--    -- Instantiate console output adapter
--    package Console is new Infrastructure.Adapter.Console_Output
--      (App_Error => App_Error, R => R.Instance);
--
--    -- Bind to Output port
--    package Output_Port is new Application.Port.Output.API
--      (App_Error => App_Error,
--       R => R.Instance,
--       Send => Console.Send);
--
--  Design Notes:
--    Infrastructure layer - concrete implementation of output capability
--
--  See Also:
--    Hybrid.Application.Port.Output - port signature this adapts to
--  ==========================================================================

with Hybrid.Domain.Foundation.Ports.Result_Port;

generic
   type App_Error is private;
   type Message_Type is private;

   with package R is new
     Hybrid.Domain.Foundation.Ports.Result_Port
       (T      => Message_Type,
        E      => App_Error,
        others => <>);

   with function To_Message (S : String) return Message_Type is <>;
   IO_Failure : App_Error;

package Hybrid.Infrastructure.Adapter.Console_Output
is

   -- Send message to console output
   -- Returns Ok(message) on success, Err(IO_Failure) on failure
   function Send (Msg : String) return R.Result;

end Hybrid.Infrastructure.Adapter.Console_Output;
