pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Presentation.Exit_Code
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Presentation layer component providing core functionality.
--
--  Usage:
--    See package specification for detailed usage examples.
--
--  Design Notes:
--    Part of the Hybrid Architecture reference application
--
--  See Also:
--    Other Presentation layer packages
--  ==========================================================================

package Hybrid.Presentation.Exit_Code is

   -- Exit code type
   type Exit_Code_Type is new Integer range 0 .. 255;

   -- Standard exit codes
   Success     : constant Exit_Code_Type := 0;   -- Successful termination
   Error       : constant Exit_Code_Type := 1;   -- General error
   Interrupted : constant Exit_Code_Type := 130; -- Interrupted by signal (128 + 2 for SIGINT)
   Usage       : constant Exit_Code_Type := 64;  -- Command line usage error
   Data_Err    : constant Exit_Code_Type := 65;  -- Data format error
   No_Input    : constant Exit_Code_Type := 66;  -- Cannot open input
   No_User     : constant Exit_Code_Type := 67;  -- Addressee unknown
   No_Host     : constant Exit_Code_Type := 68;  -- Host name unknown
   Unavailable : constant Exit_Code_Type := 69;  -- Service unavailable
   Software    : constant Exit_Code_Type := 70;  -- Internal software error
   OS_Err      : constant Exit_Code_Type := 71;  -- System error (OS)
   OS_File     : constant Exit_Code_Type := 72;  -- Critical OS file missing
   Cant_Create : constant Exit_Code_Type :=
     73;  -- Can't create (user) output file
   IO_Err      : constant Exit_Code_Type := 74;  -- Input/output error
   Temp_Fail   : constant Exit_Code_Type := 75;  -- Temp failure; retry
   Protocol    : constant Exit_Code_Type := 76;  -- Remote error in protocol
   No_Perm     : constant Exit_Code_Type := 77;  -- Permission denied
   Config      : constant Exit_Code_Type := 78;  -- Configuration error

   -- Map error types to exit codes
   function From_Domain_Error return Exit_Code_Type
   is (Data_Err);
   function From_Application_Error return Exit_Code_Type
   is (Software);
   function From_Infrastructure_Error return Exit_Code_Type
   is (IO_Err);

end Hybrid.Presentation.Exit_Code;
