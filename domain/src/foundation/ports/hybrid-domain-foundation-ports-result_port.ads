pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Foundation.Ports.Result_Port
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Port signature for Result<T,E> type. Defines the contract that
--    infrastructure adapters must satisfy.
--
--  Design Notes:
--    - Zero implementation - signature only
--    - Domain defines required operations via formal parameters
--    - Infrastructure provides concrete implementation
--    - Empty body allows library builds
--
--  See Also:
--    Hybrid.Infrastructure.Adapter.Functional.Result_API - adapter
--  ==========================================================================

generic
   type T(<>)is private;  -- Accept indefinite types (e.g., String)
   type E is private;

   --  ==========================================================================
   --  Opaque result carrier (vendor-defined in adapter)
   --  ==========================================================================

   type Result is private;

   --  ==========================================================================
   --  Constructors
   --  ==========================================================================

   pragma Warnings (Off, "* is not referenced");
   with function Ok (V : T) return Result is <>;
   with function Err (E_Val : E) return Result is <>;

   --  ==========================================================================
   --  Predicates
   --  ==========================================================================

   with function Is_Ok (R : Result) return Boolean is <>;
   with function Is_Err (R : Result) return Boolean is <>;

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   with function Value (R : Result) return T is <>;
   with function Error (R : Result) return E is <>;

   --  ==========================================================================
   --  Unwrap with defaults
   --  ==========================================================================

   with function Unwrap_Or (R : Result; Default : T) return T is <>;
   pragma Warnings (On, "* is not referenced");

package Hybrid.Domain.Foundation.Ports.Result_Port
is
   --  Signature-only generic. Empty body provided for library builds.
   --  Formal parameters are used by clients when instantiating this generic.
   --  They appear unreferenced in this spec but are referenced at instantiation.
end Hybrid.Domain.Foundation.Ports.Result_Port;
