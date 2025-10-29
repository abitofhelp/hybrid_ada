pragma Ada_2022;
--  ==========================================================================
--  Handle_Shutdown_Signal - Library-level signal handler
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Provides library-level signal handler procedure for use with
--    Hybrid.Bootstrap.Signals. Must be at library level to satisfy
--    accessibility requirements for access-to-subprogram types.
--  ==========================================================================

with Hybrid.Bootstrap.Signals;

procedure Handle_Shutdown_Signal
  (Signal    : Hybrid.Bootstrap.Signals.Signal_Type;
   Cancelled : out Boolean)
is
   pragma Unreferenced (Signal);
begin
   --  Simply set the cancellation flag
   --  Main procedure will check Is_Cancelled and log appropriately
   Cancelled := True;
end Handle_Shutdown_Signal;
