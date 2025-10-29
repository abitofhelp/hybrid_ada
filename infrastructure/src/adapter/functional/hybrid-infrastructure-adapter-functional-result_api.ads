pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Adapter.Functional.Result_API
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Infrastructure adapter that binds Functional.Result to the domain's
--    Result_Port signature.
--
--  Usage:
--    package R_Func is new
--      Hybrid.Infrastructure.Adapter.Functional.Result_API
--        (T => String, E => Error);
--
--    package My_Service is new
--      Hybrid.Domain.Service.Foo
--        (Error => Error, R => R_Func.Instance);
--
--  Design Notes:
--    - Instantiates Functional.Result with T, E
--    - Binds operations to Result_Port formal parameters
--    - Exposes port instance for domain service composition
--    - This is the ONLY file in the system that 'with's Functional
--
--  See Also:
--    Hybrid.Domain.Foundation.Ports.Result_Port - port signature
--    Functional.Result - functional library implementation
--  ==========================================================================

with Functional.Result;
with Hybrid.Domain.Foundation.Ports.Result_Port;

generic
   type T is private;
   type E is private;
package Hybrid.Infrastructure.Adapter.Functional.Result_API is

   --  Instantiate the functional library's Result type
   package FR is new Standard.Functional.Result (T => T, E => E);

   --  Bind to the domain port signature
   package Instance is new
     Hybrid.Domain.Foundation.Ports.Result_Port
       (T         => T,
        E         => E,
        Result    => FR.Result,
        Ok        => FR.Ok,
        Err       => FR.Err,
        Is_Ok     => FR.Is_Ok,
        Is_Err    => FR.Is_Err,
        Value     => FR.Value,
        Error     => FR.Error,
        Unwrap_Or => FR.Unwrap_Or);

end Hybrid.Infrastructure.Adapter.Functional.Result_API;
