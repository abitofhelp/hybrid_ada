pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Adapter.Functional.Either_API
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Infrastructure adapter that binds Functional.Either to the domain's
--    Either_Port signature.
--
--  Usage:
--    package E_Func is new
--      Hybrid.Infrastructure.Adapter.Functional.Either_API
--        (L => String, R => Integer);
--
--    package My_Service is new
--      Hybrid.Domain.Service.Foo (E => E_Func.Instance);
--
--  Design Notes:
--    - Instantiates Functional.Either with L, R
--    - Binds operations to Either_Port formal parameters
--    - Exposes port instance for domain service composition
--    - This is the ONLY file in the system that 'with's Functional
--
--  See Also:
--    Hybrid.Domain.Foundation.Ports.Either_Port - port signature
--    Functional.Either - functional library implementation
--  ==========================================================================

with Functional.Either;
with Hybrid.Domain.Foundation.Ports.Either_Port;

generic
   type L is private;
   type R is private;
package Hybrid.Infrastructure.Adapter.Functional.Either_API is

   --  Instantiate the functional library's Either type
   package FE is new Standard.Functional.Either (L => L, R => R);

   --  Bind to the domain port signature
   package Instance is new
     Hybrid.Domain.Foundation.Ports.Either_Port
       (L           => L,
        R           => R,
        Either      => FE.Either,
        Left        => FE.Left,
        Right       => FE.Right,
        Is_Left     => FE.Is_Left,
        Is_Right    => FE.Is_Right,
        Left_Value  => FE.Left_Value,
        Right_Value => FE.Right_Value);

end Hybrid.Infrastructure.Adapter.Functional.Either_API;
