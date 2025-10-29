pragma Ada_2022;
--  ==========================================================================
--  Test_Runner - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with AUnit.Run;
with AUnit.Reporter.Text;
with Test_Suite;
with Ada.Text_IO;

procedure Test_Runner is
   use Ada.Text_IO;

   procedure Runner is new AUnit.Run.Test_Runner (Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Put_Line ("=========================================");
   Put_Line ("Hybrid Architecture Ada 2022 Test Suite");
   Put_Line ("=========================================");
   New_Line;

   --  Run tests
   Runner (Reporter);

   New_Line;
   Put_Line ("Test execution completed.");
end Test_Runner;
