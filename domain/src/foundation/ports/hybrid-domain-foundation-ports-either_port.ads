pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Foundation.Ports.Either_Port
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Port signature for Either<L,R> type. Defines the contract that
--    infrastructure adapters must satisfy.
--
--  Design Notes:
--    - Zero implementation - signature only
--    - Domain defines required operations via formal parameters
--    - Infrastructure provides concrete implementation
--    - Empty body allows library builds
--
--  See Also:
--    Hybrid.Infrastructure.Adapter.Functional.Either_API - adapter
--  ==========================================================================

generic
   type L(<>)is private;  -- Accept indefinite types
   type R(<>)is private;  -- Accept indefinite types

   --  ==========================================================================
   --  Opaque either carrier (vendor-defined in adapter)
   --  ==========================================================================

   type Either is private;

   --  ==========================================================================
   --  Constructors
   --  ==========================================================================

   pragma Warnings (Off, "* is not referenced");
   with function Left (V : L) return Either is <>;
   with function Right (V : R) return Either is <>;

   --  ==========================================================================
   --  Predicates
   --  ==========================================================================

   with function Is_Left (E : Either) return Boolean is <>;
   with function Is_Right (E : Either) return Boolean is <>;

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   with function Left_Value (E : Either) return L is <>;
   with function Right_Value (E : Either) return R is <>;
   pragma Warnings (On, "* is not referenced");

package Hybrid.Domain.Foundation.Ports.Either_Port
is
   --  Signature-only generic. Empty body provided for library builds.
   --  Formal parameters are used by clients when instantiating this generic.
   --  They appear unreferenced in this spec but are referenced at instantiation.
end Hybrid.Domain.Foundation.Ports.Either_Port;
