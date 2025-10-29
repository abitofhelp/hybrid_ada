pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Logger.Concurrent
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Concurrent logging infrastructure using protected objects. Provides
--    thread-safe logging for multi-task applications.
--
--  Usage:
--    See package specification for detailed usage examples.
--
--  Design Notes:
--    Infrastructure layer - implements technical capabilities
--    Concurrent design using Ada tasks/protected objects
--
--  See Also:
--    Other Infrastructure layer packages
--  ==========================================================================

with Ada.Finalization;

package Hybrid.Infrastructure.Logger.Concurrent is

   -- Log message type
   type Log_Message is record
      Level   : Log_Level;
      Message : Unbounded_String;
      Time    : Ada.Calendar.Time;
   end record;

   -- Async logger task
   task type Logger_Task is
      entry Log (Msg : Log_Message);
      entry Set_Level (Level : Log_Level);
      entry Shutdown;
   end Logger_Task;

   type Logger_Task_Access is access Logger_Task;

   -- Concurrent logger type
   type Concurrent_Logger is new Ada.Finalization.Limited_Controlled
   with record
      Task_Logger : Logger_Task_Access;
      Min_Level   : Log_Level := Info;
   end record;

   -- Constructor
   function Create (Level : Log_Level := Info) return Concurrent_Logger;

   -- Logging methods
   procedure Debug (Self : in out Concurrent_Logger; Message : String);
   procedure Info (Self : in out Concurrent_Logger; Message : String);
   procedure Warning (Self : in out Concurrent_Logger; Message : String);
   procedure Error (Self : in out Concurrent_Logger; Message : String);

   -- Level management
   procedure Set_Level (Self : in out Concurrent_Logger; Level : Log_Level);
   function Get_Level (Self : Concurrent_Logger) return Log_Level;

   -- Lifecycle
   overriding
   procedure Initialize (Object : in out Concurrent_Logger);
   overriding
   procedure Finalize (Object : in out Concurrent_Logger);

end Hybrid.Infrastructure.Logger.Concurrent;
