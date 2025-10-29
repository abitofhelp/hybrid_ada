pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application - Application layer parent package
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Parent package for the Application layer, which orchestrates domain logic
--    through use cases and services. This layer depends only on the Domain
--    layer and defines port interfaces for infrastructure adapters.
--
--  Design Notes:
--    - Depends on Domain layer only
--    - Defines output ports (interfaces) for infrastructure adapters
--    - Contains use cases that orchestrate domain services
--    - Uses generic-based dependency injection for port binding
--  ==========================================================================

package Hybrid.Application
   with Preelaborate
is
end Hybrid.Application;
