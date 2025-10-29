pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure - Infrastructure layer parent package
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Parent package for the Infrastructure layer, which provides concrete
--    implementations of adapters for external systems (console, file I/O,
--    databases, etc.). This layer implements the port interfaces defined by
--    the Application layer.
--
--  Design Notes:
--    - Implements Application layer ports (adapters pattern)
--    - Contains concrete implementations for external dependencies
--    - Provides logger, console output, and other infrastructure services
--    - Depends on Domain and Application layers
--  ==========================================================================

package Hybrid.Infrastructure is
end Hybrid.Infrastructure;
