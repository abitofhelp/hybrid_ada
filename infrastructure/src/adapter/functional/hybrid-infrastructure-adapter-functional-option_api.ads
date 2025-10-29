pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Adapter.Functional.Option_API
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Infrastructure adapter that binds Functional.Option to the domain's
--    Option_Port signature.
--
--  Usage:
--    package O_Func is new
--      Hybrid.Infrastructure.Adapter.Functional.Option_API (T => String);
--
--    package My_Service is new
--      Hybrid.Domain.Service.Foo (O => O_Func.Instance);
--
--  Design Notes:
--    - Instantiates Functional.Option with T
--    - Binds operations to Option_Port formal parameters
--    - Exposes port instance for domain service composition
--    - This is the ONLY file in the system that 'with's Functional
--
--  See Also:
--    Hybrid.Domain.Foundation.Ports.Option_Port - port signature
--    Functional.Option - functional library implementation
--  ==========================================================================

with Functional.Option;
with Hybrid.Domain.Foundation.Ports.Option_Port;

generic
   type T is private;
package Hybrid.Infrastructure.Adapter.Functional.Option_API is

   --  Instantiate the functional library's Option type
   package FO is new Standard.Functional.Option (T => T);

   --  Bind to the domain port signature
   package Instance is new
     Hybrid.Domain.Foundation.Ports.Option_Port
       (T         => T,
        Option    => FO.Option,
        New_Some  => FO.New_Some,
        None      => FO.None,
        Is_Some   => FO.Is_Some,
        Is_None   => FO.Is_None,
        Value     => FO.Value,
        Unwrap_Or => FO.Unwrap_Or);

end Hybrid.Infrastructure.Adapter.Functional.Option_API;
