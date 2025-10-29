pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Logger
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Logging infrastructure for the application. Provides structured logging capabilities.
--
--  Usage:
--    See package specification for detailed usage examples.
--
--  Design Notes:
--    Infrastructure layer - implements technical capabilities
--    Uses Ada 2022 contracts (Pre/Post) for design-by-contract
--
--  See Also:
--    Hybrid.Domain.Model.Result - dependency
--  ==========================================================================

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Calendar;
with Hybrid.Infrastructure.Adapter.Functional.Result_API;

package Hybrid.Infrastructure.Logger is

   type Log_Level is (Debug, Info, Warning, Error);

   -- Logger error types
   type Logger_Error is record
      Message : Unbounded_String;
      Level   : Log_Level;
      Code    : Natural;  -- Error code for categorization
   end record;

   -- Error codes
   IO_Error         : constant Natural := 1;
   Task_Error       : constant Natural := 2;
   Queue_Full       : constant Natural := 3;
   Not_Initialized  : constant Natural := 4;
   Already_Shutdown : constant Natural := 5;

   -- Result type for logger operations
   -- Infrastructure can directly use adapters (not a domain service)
   package Logger_Result_API is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Boolean,
        E => Logger_Error);

   --  Use the adapter's Result directly
   package Logger_Result renames Logger_Result_API.FR;
   subtype Result is Logger_Result.Result;

   -- Safe logger with error handling (renamed from Bootstrap_Logger)
   type Bootstrap_Logger is
     new Ada.Finalization.Limited_Controlled with private;

   -- Constructor
   function Create (Level : Log_Level := Info) return Bootstrap_Logger;

   -- Logging methods that return Result
   function Debug
     (Self : in out Bootstrap_Logger; Message : String) return Result;
   function Info
     (Self : in out Bootstrap_Logger; Message : String) return Result;
   function Warning
     (Self : in out Bootstrap_Logger; Message : String) return Result;
   function Error
     (Self : in out Bootstrap_Logger; Message : String) return Result;

   -- Legacy logging methods (ignore errors)
   procedure Debug (Self : in out Bootstrap_Logger; Message : String)
   with Pre => Message'Length > 0;

   procedure Info (Self : in out Bootstrap_Logger; Message : String)
   with Pre => Message'Length > 0;

   procedure Warning (Self : in out Bootstrap_Logger; Message : String)
   with Pre => Message'Length > 0;

   procedure Error (Self : in out Bootstrap_Logger; Message : String)
   with Pre => Message'Length > 0;

   -- Try to log without waiting (non-blocking)
   function Try_Log
     (Self : in out Bootstrap_Logger; Level : Log_Level; Message : String)
      return Result;

   -- Level management
   function Set_Level
     (Self : in out Bootstrap_Logger; Level : Log_Level) return Result;
   function Get_Level (Self : Bootstrap_Logger) return Log_Level;

   -- Lifecycle management
   function Shutdown (Self : in out Bootstrap_Logger) return Result;
   function Is_Running (Self : Bootstrap_Logger) return Boolean;

   -- Get last error if any
   function Last_Error (Self : Bootstrap_Logger) return Logger_Error;

private

   -- Log entry for queuing
   type Log_Entry is record
      Level     : Log_Level;
      Message   : Unbounded_String;
      Timestamp : Ada.Calendar.Time;
   end record;

   -- Bounded queue for log entries
   type Log_Queue is array (1 .. 1000) of Log_Entry;

   -- Protected queue for thread-safe operations
   protected type Protected_Queue is
      entry Put (Entry_Rec : Log_Entry);
      entry Get (Entry_Rec : out Log_Entry);
      procedure Try_Put (Entry_Rec : Log_Entry; Success : out Boolean);
      procedure Shutdown;
      function Is_Shutting_Down return Boolean;
      function Is_Full return Boolean;
      function Count return Natural;
   private
      Queue : Log_Queue;
      Head, Tail : Natural := 1;
      Size : Natural := 0;
      Shutting_Down : Boolean := False;
   end Protected_Queue;

   type Protected_Queue_Access is access Protected_Queue;

   -- Logger task with exception safety
   task type Logger_Task (Queue : Protected_Queue_Access) is
      entry Initialize (Level : Log_Level);
      entry Set_Level (Level : Log_Level);
      entry Shutdown;
      entry Get_Status (Running : out Boolean; Error : out Logger_Error);
   end Logger_Task;

   type Logger_Task_Access is access Logger_Task;

   type Bootstrap_Logger is new Ada.Finalization.Limited_Controlled with record
      Task_Logger : Logger_Task_Access;
      Queue       : Protected_Queue_Access;
      Min_Level   : Log_Level := Info;
      Last_Err    : Logger_Error;
      Running     : Boolean := False;
   end record;

   overriding
   procedure Initialize (Object : in out Bootstrap_Logger);
   overriding
   procedure Finalize (Object : in out Bootstrap_Logger);

end Hybrid.Infrastructure.Logger;
