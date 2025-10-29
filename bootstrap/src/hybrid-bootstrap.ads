pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Bootstrap
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Application bootstrap and initialization logic. Composition root for dependency injection.
--
--  Usage:
--    See package specification for detailed usage examples.
--
--  Design Notes:
--    Pure package - no state, can be preelaborated
--
--  See Also:
--    Other Bootstrap layer packages
--  ==========================================================================

package Hybrid.Bootstrap
  with Elaborate_Body
is
   -- Bootstrap layer for application composition root
   --
   -- This package serves as the composition root where all architectural
   -- layers are wired together following dependency injection principles.

end Hybrid.Bootstrap;
