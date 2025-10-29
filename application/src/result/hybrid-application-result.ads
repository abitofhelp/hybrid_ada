pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Result - Result Port Re-Export (Parent)
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Application-level parent package for Result port re-export.
--    The actual generic signature is in child package Result_Port.
--
--  Architecture Note:
--    This facade allows Presentation to use Result<T,E> semantics
--    without violating the dependency rule (Presentation → Application only).
--    Presentation should never import Hybrid.Domain.* directly.
--
--  See Also:
--    Hybrid.Application.Result.Result_Port - child generic package
--    Hybrid.Domain.Foundation.Ports.Result_Port - underlying port
--    Hybrid.Presentation.CLI - consumer of this facade
--  ==========================================================================

package Hybrid.Application.Result is
   --  Parent package for Result port facade
   --  See child package Result_Port for the generic signature

end Hybrid.Application.Result;
