pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Logger.Concurrent - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with Ada.Text_IO;
with Ada.Calendar.Formatting;

package body Hybrid.Infrastructure.Logger.Concurrent is

   task body Logger_Task is
      Current_Level : Log_Level := Info;
      Running       : Boolean := True;

      procedure Write_Log (Msg : Log_Message) is
         Level_String : constant String :=
           (case Msg.Level is
              when Debug => "DEBUG",
              when Info => "INFO ",
              when Warning => "WARN ",
              when Error => "ERROR");
         Timestamp    : constant String :=
           Ada.Calendar.Formatting.Image (Msg.Time);
      begin
         -- Educational Note: Log_Level enumeration is intentionally ordered
         -- (Debug < Info < Warning < Error) for severity comparison.
         -- The warning about unordered enumeration can be safely suppressed.
         pragma Warnings (Off, "comparison on unordered enumeration type");
         if Msg.Level >= Current_Level then
            pragma Warnings (On, "comparison on unordered enumeration type");
            Ada.Text_IO.Put_Line
              ("["
               & Timestamp
               & "] ["
               & Level_String
               & "] "
               & To_String (Msg.Message));
            Ada.Text_IO.Flush;
         end if;
      end Write_Log;

   begin
      while Running loop
         select
            accept Log (Msg : Log_Message) do
               Write_Log (Msg);
            end Log;
         or
            accept Set_Level (Level : Log_Level) do
               Current_Level := Level;
            end Set_Level;
         or
            accept Shutdown do
               Running := False;
            end Shutdown;
         or
            terminate;
         end select;
      end loop;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("[ERROR] Logger task crashed");
   end Logger_Task;

   --  Create a new concurrent logger with specified log level.
   --  Uses Ada 2005+ extended return for limited type (build-in-place).
   --
   --  Educational Note: Limited types (like Limited_Controlled) cannot be
   --  copied in Ada. The extended return statement builds the object directly
   --  at the caller's location, avoiding the need to copy.
   function Create (Level : Log_Level := Info) return Concurrent_Logger is
   begin
      return Result : Concurrent_Logger do
         Result.Min_Level := Level;
         Result.Task_Logger := new Logger_Task;
         Result.Task_Logger.Set_Level (Level);
      end return;
   end Create;

   procedure Log_Message_Internal
     (Self : in out Concurrent_Logger; Level : Log_Level; Message : String)
   is
      Msg : constant Log_Message :=
        (Level   => Level,
         Message => To_Unbounded_String (Message),
         Time    => Ada.Calendar.Clock);
   begin
      if Self.Task_Logger /= null then
         select
            Self.Task_Logger.Log (Msg);
         or
            delay 0.1; -- Timeout to prevent blocking
            Ada.Text_IO.Put_Line
              ("[WARN] Logger busy, message dropped: " & Message);
         end select;
      end if;
   end Log_Message_Internal;

   procedure Debug (Self : in out Concurrent_Logger; Message : String) is
   begin
      Log_Message_Internal (Self, Debug, Message);
   end Debug;

   procedure Info (Self : in out Concurrent_Logger; Message : String) is
   begin
      Log_Message_Internal (Self, Info, Message);
   end Info;

   procedure Warning (Self : in out Concurrent_Logger; Message : String) is
   begin
      Log_Message_Internal (Self, Warning, Message);
   end Warning;

   procedure Error (Self : in out Concurrent_Logger; Message : String) is
   begin
      Log_Message_Internal (Self, Error, Message);
   end Error;

   procedure Set_Level (Self : in out Concurrent_Logger; Level : Log_Level) is
   begin
      Self.Min_Level := Level;
      if Self.Task_Logger /= null then
         Self.Task_Logger.Set_Level (Level);
      end if;
   end Set_Level;

   function Get_Level (Self : Concurrent_Logger) return Log_Level
   is (Self.Min_Level);

   overriding
   procedure Initialize (Object : in out Concurrent_Logger) is
   begin
      Object.Task_Logger := new Logger_Task;
      Object.Task_Logger.Set_Level (Object.Min_Level);
   end Initialize;

   overriding
   procedure Finalize (Object : in out Concurrent_Logger) is
   begin
      if Object.Task_Logger /= null then
         begin
            Object.Task_Logger.Shutdown;
         exception
            when Tasking_Error =>
               null; -- Task already terminated
         end;
      end if;
   end Finalize;

end Hybrid.Infrastructure.Logger.Concurrent;
