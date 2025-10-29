pragma Ada_2022;
--  ==========================================================================
--  Test_Presentation_CLI
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Command-line interface for the application. Handles argument parsing,
--    validation, and dependency wiring. Primary adapter in hexagonal
--    architecture.
--
--  Usage:
--    Create CLI application:
--       App : CLI_Application := Create (Use_Case'Access, Output'Access);
--       -- CLI handles argument parsing and execution
--
--  Design Notes:
--    Test file using AUnit framework
--    Tests executed via test_runner
--
--  See Also:
--    Other Presentation layer packages
--  ==========================================================================

with AUnit;
with AUnit.Simple_Test_Cases;

package Test_Presentation_CLI is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;

   overriding procedure Run_Test (T : in out Test);

end Test_Presentation_CLI;
