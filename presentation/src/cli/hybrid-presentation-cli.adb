pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Presentation.CLI - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with Ada.Command_Line;
with Ada.Text_IO;

package body Hybrid.Presentation.CLI is

   function Parse_Args return CLI_Args is
      Args : CLI_Args;
      I    : Natural := 1;
   begin
      while I <= Ada.Command_Line.Argument_Count loop
         declare
            Arg : constant String := Ada.Command_Line.Argument (I);
         begin
            if Arg = "--help" or Arg = "-h" then
               Args.Help := True;
            elsif Arg = "--version" or Arg = "-v" then
               Args.Version := True;
            elsif Arg = "--quiet" or Arg = "-q" then
               Args.Quiet := True;
            elsif Arg'Length > 0 and then Arg (Arg'First) /= '-' then
               -- Store name in fixed-length string
               if Arg'Length <= Args.Name'Length then
                  Args.Name (1 .. Arg'Length) := Arg;
                  Args.Name_Length := Arg'Length;
               end if;
            end if;
         end;
         I := I + 1;
      end loop;

      return Args;
   end Parse_Args;

   procedure Show_Help is
   begin
      Ada.Text_IO.Put_Line (App_Name & " v" & App_Version);
      Ada.Text_IO.Put_Line ("A demonstration of hybrid architecture in Ada");
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.Put_Line
        ("  " & Ada.Command_Line.Command_Name & " [options] <name>");
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Put_Line ("Options:");
      Ada.Text_IO.Put_Line ("  -h, --help     Show this help message");
      Ada.Text_IO.Put_Line ("  -v, --version  Show version information");
      Ada.Text_IO.Put_Line ("  -q, --quiet    Suppress greeting output");
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Put_Line ("Arguments:");
      Ada.Text_IO.Put_Line ("  <name>         The name to greet");
   end Show_Help;

   procedure Show_Version is
   begin
      Ada.Text_IO.Put_Line (App_Name & " version " & App_Version);
   end Show_Version;

   function Run return Exit_Code.Exit_Code_Type is
      use Ada.Command_Line;
      use Ada.Text_IO;

      Args : constant CLI_Args := Parse_Args;
   begin
      -- Handle help flag
      if Args.Help then
         Show_Help;
         return Exit_Code.Success;
      end if;

      -- Handle version flag
      if Args.Version then
         Show_Version;
         return Exit_Code.Success;
      end if;

      -- Check for name argument
      if Args.Name_Length = 0 then
         Put_Line ("Error: Missing required argument <name>");
         Put_Line ("Try '" & Command_Name & " --help' for more information.");
         return Exit_Code.Usage;
      end if;

      -- Execute service
      declare
         Name   : constant String := Args.Name (1 .. Args.Name_Length);
         Result : constant R.Result := Run_Service (Name);
      begin
         if R.Is_Ok (Result) then
            if not Args.Quiet then
               Put_Line (To_String (R.Value (Result)));
            end if;
            return Exit_Code.Success;
         else
            Put_Line ("Error: Failed to create greeting");
            return Exit_Code.Error;
         end if;
      end;

   exception
      when others =>
         Put_Line ("Fatal error occurred");
         return Exit_Code.Software;
   end Run;

end Hybrid.Presentation.CLI;
