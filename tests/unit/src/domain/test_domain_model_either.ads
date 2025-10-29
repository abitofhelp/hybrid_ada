pragma Ada_2022;
--  ==========================================================================
--  Test_Domain_Model_Either
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Test suite for Domain Model Either. Validates functionality using AUnit framework.
--
--  Usage:
--    Run via test suite:
--       ./tests/bin/test_runner
--
--       Or via Make:
--       make test
--
--  Design Notes:
--    Functional monad pattern for error handling without exceptions
--    Test file using AUnit framework
--    Tests executed via test_runner
--
--  See Also:
--    Hybrid.Domain.Error - error types
--  ==========================================================================

with AUnit;
with AUnit.Simple_Test_Cases;

package Test_Domain_Model_Either is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;

   overriding procedure Run_Test (T : in out Test);

end Test_Domain_Model_Either;
