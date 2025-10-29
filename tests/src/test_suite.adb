pragma Ada_2022;
--  ==========================================================================
--  Test_Suite - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Simple_Test_Cases;
use AUnit.Simple_Test_Cases;
-- Domain layer unit tests
with Test_Domain_Value_Person_Name;
--  NOTE: Result and Either monads replaced with port/adapter pattern
--  with Test_Domain_Model_Result;
--  with Test_Domain_Model_Either;
with Test_Domain_Error;
with Test_Domain_Service_Greeting;
-- Application layer unit tests
with Test_Application_Error;
with Test_Application_Result;
with Test_Application_Service_Create_Greeting;
-- Infrastructure layer unit tests
with Test_Infrastructure_Adapter_Console_Output;
-- Presentation layer unit tests
with Test_Presentation_CLI;
-- Integration tests
with Test_Integration_Create_Greeting;
-- End-to-end tests
with Test_E2E_Greeting;

package body Test_Suite is

   use AUnit.Test_Suites;

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      --  Domain Layer Tests
      --  Tests value objects, domain models, errors, and domain services per DDD
      Ret.Add_Test (Test_Case_Access'(new Test_Domain_Value_Person_Name.Test));
      --  NOTE: Result and Either monads replaced with port/adapter pattern
      --  Monads are now tested via integration tests using real adapters
      --  Ret.Add_Test (Test_Case_Access'(new Test_Domain_Model_Result.Test));
      --  Ret.Add_Test (Test_Case_Access'(new Test_Domain_Model_Either.Test));
      Ret.Add_Test (Test_Case_Access'(new Test_Domain_Error.Test));
      Ret.Add_Test (Test_Case_Access'(new Test_Domain_Service_Greeting.Test));

      --  Application Layer Unit Tests
      --  Tests use cases, errors, and application services
      Ret.Add_Test (Test_Case_Access'(new Test_Application_Error.Test));
      Ret.Add_Test (Test_Case_Access'(new Test_Application_Result.Test));
      Ret.Add_Test
        (Test_Case_Access'(new Test_Application_Service_Create_Greeting.Test));

      --  Infrastructure Layer Unit Tests
      --  Tests infrastructure adapters in isolation
      Ret.Add_Test
        (Test_Case_Access'
           (new Test_Infrastructure_Adapter_Console_Output.Test));

      --  Presentation Layer Unit Tests
      --  Tests presentation layer components
      Ret.Add_Test (Test_Case_Access'(new Test_Presentation_CLI.Test));

      --  Integration Tests
      --  Tests interaction between Application and Infrastructure layers
      Ret.Add_Test
        (Test_Case_Access'(new Test_Integration_Create_Greeting.Test));

      --  End-to-End Tests
      --  Tests complete stack from domain through infrastructure
      Ret.Add_Test (Test_Case_Access'(new Test_E2E_Greeting.Test));

      return Ret;
   end Suite;

end Test_Suite;
