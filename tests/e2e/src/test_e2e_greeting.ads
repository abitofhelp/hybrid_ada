pragma Ada_2022;
--  ==========================================================================
--  Test_E2E_Greeting
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Test suite for E2E Greeting. Validates functionality using AUnit framework.
--
--  Usage:
--    Run via test suite:
--       ./tests/bin/test_runner
--
--       Or via Make:
--       make test
--
--  Design Notes:
--    Test file using AUnit framework
--    Tests executed via test_runner
--
--  See Also:
--    Other Test layer packages
--  ==========================================================================

with AUnit;
with AUnit.Simple_Test_Cases;

package Test_E2E_Greeting is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;

   overriding procedure Run_Test (T : in out Test);

end Test_E2E_Greeting;
