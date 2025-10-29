pragma Ada_2022;
--  ==========================================================================
--  Test_Application_Error
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines error types for the Application layer including Test. Provides type-safe error handling.
--
--  Usage:
--    Error types are used with Result monad for type-safe error handling:
--       Result := Create (Input);
--       if not Is_Ok (Result) then
--          Handle_Error (Get_Err (Result));
--       end if;
--
--  Design Notes:
--    Application layer - orchestrates domain logic
--    Test file using AUnit framework
--    Tests executed via test_runner
--
--  See Also:
--    Hybrid.Application.Port.Output - output interface
--  ==========================================================================

with AUnit;
with AUnit.Simple_Test_Cases;

package Test_Application_Error is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;

   overriding procedure Run_Test (T : in out Test);

end Test_Application_Error;
