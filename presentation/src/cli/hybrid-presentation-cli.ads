pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Presentation.CLI
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Command-line interface for the application. Handles argument parsing,
--    validation, and dependency wiring. Primary adapter in hexagonal architecture.
--
--  Usage:
--    Instantiate with your service and output implementations:
--       package My_CLI is new Hybrid.Presentation.CLI
--         (App_Error => My_Error,
--          R => My_Result,
--          Run_Service => My_Service,
--          Send_Output => My_Output);
--
--  Design Notes:
--    Generic package - instantiated with specific service implementations
--    Supports flags: --help, --version, --quiet
--
--  See Also:
--    Hybrid.Application.Service.Create_Greeting - service to wrap
--    Hybrid.Presentation.Exit_Code - exit codes
--  ==========================================================================

with Hybrid.Application.Result.Result_Port;
with Hybrid.Presentation.Exit_Code;

generic
   type App_Error is private;
   type Message_Type is private;

   with package R is new
     Hybrid.Application.Result.Result_Port
       (T      => Message_Type,
        E      => App_Error,
        others => <>);

   with function Run_Service (Name : String) return R.Result is <>;
   with function Send_Output (Msg : String) return R.Result is <>;
   with function To_String (Msg : Message_Type) return String is <>;

package Hybrid.Presentation.CLI
is
   --  Send_Output is a formal parameter reserved for future use
   pragma Unreferenced (Send_Output);

   -- Run the CLI application
   function Run return Exit_Code.Exit_Code_Type;

private

   -- CLI arguments
   type CLI_Args is record
      Name    : String (1 .. 256);
      Name_Length : Natural := 0;
      Help    : Boolean := False;
      Version : Boolean := False;
      Quiet   : Boolean := False;
   end record;

   -- Parse command line arguments
   function Parse_Args return CLI_Args;

   -- Display help
   procedure Show_Help;

   -- Display version
   procedure Show_Version;

   -- Application metadata
   App_Name    : constant String := "Hybrid Architecture Template";
   App_Version : constant String := "1.0.0";

end Hybrid.Presentation.CLI;
