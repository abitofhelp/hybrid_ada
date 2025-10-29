pragma Ada_2022;
--  ==========================================================================
--  Test_Suite
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Test suite for Suite. Validates functionality using AUnit framework.
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

with AUnit.Test_Suites;

package Test_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Test_Suite;
