pragma Ada_2022;
--  ==========================================================================
--  Mock_Output_Port
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Mock implementation for testing. Provides test double for dependency injection in unit tests.
--
--  Usage:
--    See package specification for detailed usage examples.
--
--  Design Notes:
--    Port interface for hexagonal architecture
--
--  See Also:
--    Hybrid.Application.Port.Output - dependency
--  ==========================================================================

with Hybrid.Application.Port.Output;

package Mock_Output_Port is

   type Mock_Output_Port_Type is new Hybrid.Application.Port.Output.Output_Port with null record;

   overriding procedure Send
     (Self    : Mock_Output_Port_Type;
      Message : String;
      Result  : out Hybrid.Application.Port.Output.Result);

end Mock_Output_Port;
