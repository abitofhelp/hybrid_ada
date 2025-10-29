pragma Ada_2022;
--  ==========================================================================
--  Test_Application_Result
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Tests for Application.Result.Result_Port facade pattern.
--    Verifies that the facade properly re-exports Result operations and
--    successfully breaks Presentation→Domain coupling.
--
--  Design Notes:
--    - Tests facade instantiation with different types
--    - Verifies all Result operations work through facade
--    - Architectural test: ensures no Domain imports needed
--
--  See Also:
--    Hybrid.Application.Result.Result_Port - facade under test
--  ==========================================================================

with AUnit;
with AUnit.Simple_Test_Cases;

package Test_Application_Result is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;

   overriding procedure Run_Test (T : in out Test);

end Test_Application_Result;
