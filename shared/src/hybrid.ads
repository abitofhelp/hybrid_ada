pragma Ada_2022;
--  ==========================================================================
--  Hybrid - Root package
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Root package for the Hybrid Architecture Template project. This package
--    serves as the namespace root for all architectural layers: Domain,
--    Application, Infrastructure, Presentation, and Bootstrap.
--
--  Design Notes:
--    - Follows hexagonal architecture (Ports & Adapters pattern)
--    - Enforces clean separation of concerns across layers
--    - Uses generic-based dependency injection for compile-time safety
--  ==========================================================================

package Hybrid
   with Preelaborate
is
end Hybrid;
