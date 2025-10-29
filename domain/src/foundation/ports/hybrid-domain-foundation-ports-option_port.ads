pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Foundation.Ports.Option_Port
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Port signature for Option<T> type. Defines the contract that
--    infrastructure adapters must satisfy.
--
--  Design Notes:
--    - Zero implementation - signature only
--    - Domain defines required operations via formal parameters
--    - Infrastructure provides concrete implementation
--    - Empty body allows library builds
--
--  See Also:
--    Hybrid.Infrastructure.Adapter.Functional.Option_API - adapter
--  ==========================================================================

generic
   type T(<>)is private;  -- Accept indefinite types (e.g., String)

   --  ==========================================================================
   --  Opaque option carrier (vendor-defined in adapter)
   --  ==========================================================================

   type Option is private;

   --  ==========================================================================
   --  Constructors
   --  ==========================================================================

   pragma Warnings (Off, "* is not referenced");
   with function New_Some (V : T) return Option is <>;
   with function None return Option is <>;

   --  ==========================================================================
   --  Predicates
   --  ==========================================================================

   with function Is_Some (O : Option) return Boolean is <>;
   with function Is_None (O : Option) return Boolean is <>;

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   with function Value (O : Option) return T is <>;

   --  ==========================================================================
   --  Unwrap with defaults
   --  ==========================================================================

   with function Unwrap_Or (O : Option; Default : T) return T is <>;
   pragma Warnings (On, "* is not referenced");

package Hybrid.Domain.Foundation.Ports.Option_Port
is
   --  Signature-only generic. Empty body provided for library builds.
   --  Formal parameters are used by clients when instantiating this generic.
   --  They appear unreferenced in this spec but are referenced at instantiation.
end Hybrid.Domain.Foundation.Ports.Option_Port;
