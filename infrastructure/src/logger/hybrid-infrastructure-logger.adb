pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Logger - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

with Ada.Text_IO;
with Ada.Calendar.Formatting;
with Ada.Exceptions;

package body Hybrid.Infrastructure.Logger is

   use Logger_Result;

   protected body Protected_Queue is

      entry Put (Entry_Rec : Log_Entry)
        when Size < Log_Queue'Length and not Shutting_Down
      is
      begin
         Queue (Tail) := Entry_Rec;
         Tail := (Tail mod Log_Queue'Length) + 1;
         Size := Size + 1;
      end Put;

      entry Get (Entry_Rec : out Log_Entry) when Size > 0 is
      begin
         Entry_Rec := Queue (Head);
         Head := (Head mod Log_Queue'Length) + 1;
         Size := Size - 1;
      end Get;

      procedure Try_Put (Entry_Rec : Log_Entry; Success : out Boolean) is
      begin
         if Size < Log_Queue'Length and not Shutting_Down then
            Queue (Tail) := Entry_Rec;
            Tail := (Tail mod Log_Queue'Length) + 1;
            Size := Size + 1;
            Success := True;
         else
            Success := False;
         end if;
      end Try_Put;

      procedure Shutdown is
      begin
         Shutting_Down := True;
      end Shutdown;

      function Is_Shutting_Down return Boolean
      is (Shutting_Down);
      function Is_Full return Boolean
      is (Size = Log_Queue'Length);
      function Count return Natural
      is (Size);

   end Protected_Queue;

   task body Logger_Task is
      Current_Level : Log_Level := Info;
      Task_Running  : Boolean := True;
      Task_Error    : Logger_Error :=
        (Message => To_Unbounded_String (""), Level => Info, Code => 0);

      procedure Write_Log_Entry (Entry_Rec : Log_Entry) is
         Level_String : constant String :=
           (case Entry_Rec.Level is
              when Debug => "DEBUG",
              when Info => "INFO ",
              when Warning => "WARN ",
              when Error => "ERROR");
         Timestamp    : constant String :=
           Ada.Calendar.Formatting.Image (Entry_Rec.Timestamp);
      begin
         if Entry_Rec.Level >= Current_Level then
            Ada.Text_IO.Put_Line
              ("["
               & Timestamp
               & "] ["
               & Level_String
               & "] "
               & To_String (Entry_Rec.Message));
            Ada.Text_IO.Flush;
         end if;
      exception
         when E : Ada.Text_IO.Use_Error =>
            Task_Error :=
              (Message =>
                 To_Unbounded_String
                   ("I/O error: " & Ada.Exceptions.Exception_Message (E)),
               Level   => Infrastructure.Logger.Error,
               Code    => IO_Error);
         when E : others =>
            Task_Error :=
              (Message =>
                 To_Unbounded_String
                   ("Unexpected error: "
                    & Ada.Exceptions.Exception_Message (E)),
               Level   => Infrastructure.Logger.Error,
               Code    => IO_Error);
      end Write_Log_Entry;

   begin
      -- Wait for initialization
      accept Initialize (Level : Log_Level) do
         Current_Level := Level;
      end Initialize;

      -- Main processing loop
      while Task_Running loop
         select
            accept Set_Level (Level : Log_Level) do
               Current_Level := Level;
            end Set_Level;
         or
            accept Shutdown do
               Task_Running := False;
            end Shutdown;
         or
            accept Get_Status
              (Running : out Boolean; Error : out Logger_Error)
            do
               Running := Task_Running;
               Error := Task_Error;
            end Get_Status;
         else
            -- Process log entries
            declare
               Entry_Rec : Log_Entry;
            begin
               select
                  Queue.Get (Entry_Rec);
                  Write_Log_Entry (Entry_Rec);
               or
                  delay 0.01; -- Small delay to prevent busy waiting
               end select;
            exception
               when E : others =>
                  Task_Error :=
                    (Message =>
                       To_Unbounded_String
                         ("Queue error: "
                          & Ada.Exceptions.Exception_Message (E)),
                     Level   => Infrastructure.Logger.Error,
                     Code    => Task_Error.Code);
            end;
         end select;
      end loop;

      -- Drain remaining entries
      while Queue.Count > 0 loop
         declare
            Entry_Rec : Log_Entry;
         begin
            Queue.Get (Entry_Rec);
            Write_Log_Entry (Entry_Rec);
         exception
            when others =>
               exit;
         end;
      end loop;

   exception
      when E : others =>
         Task_Error :=
           (Message =>
              To_Unbounded_String
                ("Fatal task error: " & Ada.Exceptions.Exception_Message (E)),
            Level   => Infrastructure.Logger.Error,
            Code    => Task_Error.Code);
   end Logger_Task;

   function Create (Level : Log_Level := Info) return Bootstrap_Logger is
   begin
      return Result : Bootstrap_Logger do
         Result.Min_Level := Level;
         Result.Initialize;
      end return;
   end Create;

   function Log_Internal
     (Self : in out Bootstrap_Logger; Level : Log_Level; Message : String)
      return Result
   is
      Entry_Rec : constant Log_Entry :=
        (Level     => Level,
         Message   => To_Unbounded_String (Message),
         Timestamp => Ada.Calendar.Clock);
   begin
      if not Self.Running then
         Self.Last_Err :=
           (Message => To_Unbounded_String ("Logger not initialized"),
            Level   => Infrastructure.Logger.Error,
            Code    => Not_Initialized);
         return Err (Self.Last_Err);
      end if;

      begin
         Self.Queue.Put (Entry_Rec);
         return Ok (True);
      exception
         when Program_Error =>
            -- Queue is full
            Self.Last_Err :=
              (Message => To_Unbounded_String ("Log queue full"),
               Level   => Warning,
               Code    => Queue_Full);
            return Err (Self.Last_Err);
         when E : others =>
            Self.Last_Err :=
              (Message =>
                 To_Unbounded_String (Ada.Exceptions.Exception_Message (E)),
               Level   => Infrastructure.Logger.Error,
               Code    => IO_Error);
            return Err (Self.Last_Err);
      end;
   end Log_Internal;

   function Debug
     (Self : in out Bootstrap_Logger; Message : String) return Result is
   begin
      return Log_Internal (Self, Debug, Message);
   end Debug;

   function Info
     (Self : in out Bootstrap_Logger; Message : String) return Result is
   begin
      return Log_Internal (Self, Info, Message);
   end Info;

   function Warning
     (Self : in out Bootstrap_Logger; Message : String) return Result is
   begin
      return Log_Internal (Self, Warning, Message);
   end Warning;

   function Error
     (Self : in out Bootstrap_Logger; Message : String) return Result is
   begin
      return Log_Internal (Self, Error, Message);
   end Error;

   -- Legacy procedures that ignore errors
   procedure Debug (Self : in out Bootstrap_Logger; Message : String) is
      Res : Result;
   begin
      Res := Debug (Self, Message);
   end Debug;

   procedure Info (Self : in out Bootstrap_Logger; Message : String) is
      Res : Result;
   begin
      Res := Info (Self, Message);
   end Info;

   procedure Warning (Self : in out Bootstrap_Logger; Message : String) is
      Res : Result;
   begin
      Res := Warning (Self, Message);
   end Warning;

   procedure Error (Self : in out Bootstrap_Logger; Message : String) is
      Res : Result;
   begin
      Res := Error (Self, Message);
   end Error;

   function Try_Log
     (Self : in out Bootstrap_Logger; Level : Log_Level; Message : String)
      return Result
   is
      Entry_Rec : constant Log_Entry :=
        (Level     => Level,
         Message   => To_Unbounded_String (Message),
         Timestamp => Ada.Calendar.Clock);
   begin
      if not Self.Running then
         return
           Err
             ((Message => To_Unbounded_String ("Logger not running"),
               Level   => Error,
               Code    => Not_Initialized));
      end if;

      declare
         Success : Boolean;
      begin
         Self.Queue.Try_Put (Entry_Rec, Success);
         if Success then
            return Ok (True);
         else
            return
              Err
                ((Message =>
                    To_Unbounded_String ("Queue full or shutting down"),
                  Level   => Warning,
                  Code    => Queue_Full));
         end if;
      end;
   end Try_Log;

   function Set_Level
     (Self : in out Bootstrap_Logger; Level : Log_Level) return Result is
   begin
      Self.Min_Level := Level;
      if Self.Task_Logger /= null then
         begin
            Self.Task_Logger.Set_Level (Level);
            return Ok (True);
         exception
            when E : others =>
               return
                 Err
                   ((Message =>
                       To_Unbounded_String
                         ("Failed to set level: "
                          & Ada.Exceptions.Exception_Message (E)),
                     Level   => Infrastructure.Logger.Error,
                     Code    => Task_Error));
         end;
      end if;
      return Ok (True);
   end Set_Level;

   function Get_Level (Self : Bootstrap_Logger) return Log_Level
   is (Self.Min_Level);

   function Shutdown (Self : in out Bootstrap_Logger) return Result is
      Running : Boolean;
      Error   : Logger_Error;
   begin
      if not Self.Running then
         return Ok (True);
      end if;

      -- Shutdown queue first
      Self.Queue.Shutdown;

      -- Then shutdown task
      begin
         Self.Task_Logger.Shutdown;

         -- Get final status
         select
            Self.Task_Logger.Get_Status (Running, Error);
            if Error.Code /= 0 then
               return Err (Error);
            end if;
         or
            delay 2.0;
            return
              Err
                ((Message =>
                    To_Unbounded_String
                      ("Timeout waiting for logger shutdown"),
                  Level   => Infrastructure.Logger.Error,
                  Code    => Task_Error));
         end select;
      exception
         when Tasking_Error =>
            null; -- Task already terminated
         when E : others =>
            return
              Err
                ((Message =>
                    To_Unbounded_String
                      ("Error during shutdown: "
                       & Ada.Exceptions.Exception_Message (E)),
                  Level   => Infrastructure.Logger.Error,
                  Code    => Task_Error));
      end;

      Self.Running := False;
      return Ok (True);
   end Shutdown;

   function Is_Running (Self : Bootstrap_Logger) return Boolean
   is (Self.Running);

   function Last_Error (Self : Bootstrap_Logger) return Logger_Error
   is (Self.Last_Err);

   overriding
   procedure Initialize (Object : in out Bootstrap_Logger) is
   begin
      Object.Queue := new Protected_Queue;
      Object.Task_Logger := new Logger_Task (Object.Queue);
      Object.Task_Logger.Initialize (Object.Min_Level);
      Object.Running := True;
   exception
      when E : others =>
         Object.Last_Err :=
           (Message =>
              To_Unbounded_String
                ("Failed to initialize: "
                 & Ada.Exceptions.Exception_Message (E)),
            Level   => Error,
            Code    => Not_Initialized);
         Object.Running := False;
   end Initialize;

   overriding
   procedure Finalize (Object : in out Bootstrap_Logger) is
      Res : Result;
   begin
      if Object.Running then
         Res := Object.Shutdown;
      end if;
   end Finalize;

end Hybrid.Infrastructure.Logger;
