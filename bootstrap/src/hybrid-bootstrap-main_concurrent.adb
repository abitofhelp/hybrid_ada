pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Bootstrap.Main_Concurrent - Concurrent/Tasking Mode
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Application bootstrap using task-based concurrency. Wires together all
--    layers using dependency injection via generic instantiation.
--
--  Design Notes:
--    - Uses Application_Runner task for concurrent execution
--    - Main thread spawns task and waits for completion
--    - Error handling: task reports errors back to main via rendezvous
--    - Signal handling: graceful shutdown across tasks
--  ==========================================================================

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Calendar.Formatting;
with Hybrid.Presentation.Exit_Code;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;
with Hybrid.Infrastructure.Adapter.Console_Output;
with Hybrid.Domain.Value.Person_Name;
with Hybrid.Domain.Value.Person_Name.Operations;
with Hybrid.Application.Service.Create_Greeting;
with Hybrid.Application.Port.Output;
with Hybrid.Application.Result.Result_Port;
with Hybrid.Application.Types;
with Hybrid.Application.Error;
with Hybrid.Domain.Error;
with Hybrid.Presentation.CLI;
with Hybrid.Bootstrap.Signals;
with Handle_Shutdown_Signal;

procedure Hybrid.Bootstrap.Main_Concurrent is

   --  =======================================================================
   --  Step 1: Type Aliases for Clarity
   --  =======================================================================

   package App_Err renames Hybrid.Application.Error;
   package Dom_Err renames Hybrid.Domain.Error;

   --  =======================================================================
   --  Step 2: Instantiate Result Adapters
   --  Note: Using tagged error types for rich context
   --  =======================================================================

   package App_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Application.Types.Message_Type,
        E => App_Err.Application_Error);

   package Domain_Result is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Hybrid.Domain.Value.Person_Name.Person_Name,
        E => Dom_Err.Domain_Error);

   --  =======================================================================
   --  Step 3: Instantiate Person_Name Operations
   --  =======================================================================

   package Person_Name_Ops is new
     Hybrid.Domain.Value.Person_Name.Operations.API
       (R => Domain_Result.Instance);

   --  =======================================================================
   --  Step 4: Instantiate Console Output Adapter
   --  =======================================================================

   --  IO failure error for Console adapter
   Console_IO_Error : constant App_Err.Application_Error :=
     App_Err.Output_Error
       (Use_Case    => "Console_Output",
        Output_Type => "stdout",
        Reason      => "Device unavailable or write failed");

   package Console_Out is new
     Hybrid.Infrastructure.Adapter.Console_Output
       (App_Error    => App_Err.Application_Error,
        Message_Type => Hybrid.Application.Types.Message_Type,
        R            => App_Result.Instance,
        To_Message   => Hybrid.Application.Types.To_Message,
        IO_Failure   => Console_IO_Error);

   --  =======================================================================
   --  Step 5: Adapter Wrapper and Output Port Binding
   --  =======================================================================

   function Send_Message
     (Msg : Hybrid.Application.Types.Message_Type) return App_Result.FR.Result
   is
   begin
      return Console_Out.Send (Hybrid.Application.Types.To_String (Msg));
   end Send_Message;

   package Output_Port is new
     Hybrid.Application.Port.Output.API
       (App_Error    => App_Err.Application_Error,
        Message_Type => Hybrid.Application.Types.Message_Type,
        R            => App_Result.Instance,
        Send         => Send_Message);

   --  =======================================================================
   --  Step 6: Error Mapping Function
   --  =======================================================================

   --  Maps Domain.Error → Application.Error at composition boundary
   --  Preserves Entity/Operation/Message context while adapting to app layer
   function Map_Domain_To_App (E : Dom_Err.Domain_Error)
     return App_Err.Application_Error
   is
      use Dom_Err.Error_Messages;
   begin
      return
        App_Err.New_Application_Error
          (Use_Case  => "CreateGreeting",
           Operation => To_String (E.Operation) & "_" & To_String (E.Entity),
           Message   => To_String (E.Message));
   end Map_Domain_To_App;

   --  =======================================================================
   --  Step 7: Instantiate Create_Greeting Service
   --  =======================================================================

   package Greeting_Service is new
     Hybrid.Application.Service.Create_Greeting.API
       (App_Error         => App_Err.Application_Error,
        Person_Name_Error => Dom_Err.Domain_Error,
        Message_Type      => Hybrid.Application.Types.Message_Type,
        Map_Error         => Map_Domain_To_App,
        To_Message        => Hybrid.Application.Types.To_Message,
        R                 => App_Result.Instance,
        R_Name            => Domain_Result.Instance,
        Name_Ops          => Person_Name_Ops,
        Output_Port       => Output_Port);

   --  =======================================================================
   --  Step 8: Instantiate Application Result Facade
   --  =======================================================================

   --  Create Application-layer Result port instance to break Presentation→Domain dependency
   --  This facade allows Presentation to depend only on Application, not Domain
   package App_Result_Port is new
     Hybrid.Application.Result.Result_Port
       (T         => Hybrid.Application.Types.Message_Type,
        E         => App_Err.Application_Error,
        Result    => App_Result.FR.Result,
        Ok        => App_Result.FR.Ok,
        Err       => App_Result.FR.Err,
        Is_Ok     => App_Result.FR.Is_Ok,
        Is_Err    => App_Result.FR.Is_Err,
        Value     => App_Result.FR.Value,
        Error     => App_Result.FR.Error,
        Unwrap_Or => App_Result.FR.Unwrap_Or);

   --  =======================================================================
   --  Step 9: Instantiate CLI
   --  =======================================================================

   package CLI is new
     Hybrid.Presentation.CLI
       (App_Error    => App_Err.Application_Error,
        Message_Type => Hybrid.Application.Types.Message_Type,
        R            => App_Result_Port,
        Run_Service  => Greeting_Service.Run,
        Send_Output  => Console_Out.Send,
        To_String    => Hybrid.Application.Types.To_String);

   --  =======================================================================
   --  Simple Synchronous Logging (shared by both tasks)
   --  =======================================================================
   --  Note: Using Ada.Text_IO for simplicity. The concurrent aspect is
   --  demonstrated by the Application_Runner task, not by async logging.

   procedure Log_Info (Message : String) is
      Timestamp : constant String :=
        Ada.Calendar.Formatting.Image (Ada.Calendar.Clock);
   begin
      Ada.Text_IO.Put_Line
        ("[" & Timestamp (1 .. 19) & "] [INFO ] " & Message);
   end Log_Info;

   procedure Log_Error (Message : String) is
      Timestamp : constant String :=
        Ada.Calendar.Formatting.Image (Ada.Calendar.Clock);
   begin
      Ada.Text_IO.Put_Line
        ("[" & Timestamp (1 .. 19) & "] [ERROR] " & Message);
   end Log_Error;

   --  =======================================================================
   --  Application Runner Task
   --  =======================================================================

   task type Application_Runner is
      entry Start;
      entry Get_Exit_Code (Code : out Hybrid.Presentation.Exit_Code.Exit_Code_Type);
   end Application_Runner;

   task body Application_Runner is
      use Hybrid.Presentation.Exit_Code;

      App_Exit_Code  : Exit_Code_Type;
      Signal_Manager : Signals.System_Signals := Signals.Create;
      Signal_Result  : Signals.Result;

   begin
      -- Wait for start signal
      accept Start;

      -- Log startup from task
      Log_Info
        ("Starting " & Ada.Command_Line.Command_Name & " (concurrent mode)");

      -- Install signal handlers for graceful shutdown
      Signal_Result := Signal_Manager.Install (Handle_Shutdown_Signal'Access);

      if Signals.Signal_Result.Is_Err (Signal_Result) then
         Log_Error ("Warning: Could not install signal handlers");
      else
         Log_Info ("Signal handlers installed (SIGINT, SIGTERM)");
      end if;

      -- Run CLI application
      begin
         App_Exit_Code := CLI.Run;

         -- Check if shutdown was requested via signal
         if Signal_Manager.Is_Cancelled then
            Log_Info
              ("Application interrupted by signal " &
               Signal_Manager.Last_Signal'Image);
            App_Exit_Code := Interrupted;  -- Exit code 130 for SIGINT
         else
            -- Log normal completion
            Log_Info
              ("Application completed with exit code:" & App_Exit_Code'Image);
         end if;

      exception
         when E : others =>
            Log_Error
              ("Fatal error in task: " &
               Ada.Exceptions.Exception_Message (E));
            App_Exit_Code := Software;
      end;

      -- Uninstall signal handlers before exit
      Signal_Result := Signal_Manager.Uninstall;

      if Signals.Signal_Result.Is_Ok (Signal_Result) then
         Log_Info ("Signal handlers uninstalled");
      end if;

      -- Return exit code to main thread
      accept Get_Exit_Code (Code : out Hybrid.Presentation.Exit_Code.Exit_Code_Type) do
         Code := App_Exit_Code;
      end Get_Exit_Code;

   exception
      when E : others =>
         -- Task-level exception handler
         declare
            use Ada.Text_IO;
         begin
            Put_Line
              ("Application task crashed: " &
               Ada.Exceptions.Exception_Message (E));
            accept Get_Exit_Code (Code : out Hybrid.Presentation.Exit_Code.Exit_Code_Type) do
               Code := Hybrid.Presentation.Exit_Code.Software;
            end Get_Exit_Code;
         end;
   end Application_Runner;

   --  =======================================================================
   --  Main Thread Variables
   --  =======================================================================

   Runner          : Application_Runner;
   Final_Exit_Code : Hybrid.Presentation.Exit_Code.Exit_Code_Type;

begin
   -- Start application task
   Runner.Start;

   -- Wait for completion and get exit code via rendezvous
   Runner.Get_Exit_Code (Final_Exit_Code);

   -- Set exit status
   Ada.Command_Line.Set_Exit_Status
     (Ada.Command_Line.Exit_Status (Final_Exit_Code));

exception
   when others =>
      declare
         use Ada.Text_IO;
      begin
         Put_Line ("Fatal error in main thread");
         Ada.Command_Line.Set_Exit_Status
           (Ada.Command_Line.Exit_Status (Hybrid.Presentation.Exit_Code.Software));
      end;
end Hybrid.Bootstrap.Main_Concurrent;
