pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Bootstrap.Signals - Implementation (AbohLib-based)
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Implementation Notes:
--    This implementation uses AbohLib's POSIX signal handler and cancellation
--    source pattern. Unlike the previous task-based implementation, this uses
--    only protected objects, avoiding task entry blocking issues that caused
--    program hangs on termination.
--  ==========================================================================

with AbohLib.Infrastructure.OS.Signal_Handler.POSIX;

package body Hybrid.Bootstrap.Signals is

   use AbohLib.Infrastructure.OS.Signal_Handler;
   use AbohLib.Infrastructure.Concurrent.Cancellation_Source;

   --  Map Hybrid signal types to AbohLib signal types
   --  Currently unused but retained for potential future bidirectional mapping needs
   function To_AbohLib_Signal
     (Signal : Signal_Type) return AbohLib.Infrastructure.OS.Signal_Handler.Signal_Type
   is
     (case Signal is
         when Interrupt => SIGINT,
         when Term      => SIGTERM,
         when Kill      => SIGQUIT,
         when User1     => SIGUSR1,
         when User2     => SIGUSR2,
         when Alarm     => Unknown);  --  SIGALRM not in AbohLib yet
   pragma Unreferenced (To_AbohLib_Signal);

   --  Map AbohLib signal types to Hybrid signal types
   function To_Hybrid_Signal
     (Signal : AbohLib.Infrastructure.OS.Signal_Handler.Signal_Type)
      return Signal_Type
   is
     (case Signal is
         when SIGINT  => Interrupt,
         when SIGTERM => Term,
         when SIGQUIT => Kill,
         when SIGUSR1 => User1,
         when SIGUSR2 => User2,
         when SIGHUP  => Term,  --  Treat HUP like TERM
         when Unknown => Interrupt);  --  Default to Interrupt

   function Make_Error
     (Message : String; Signal : Signal_Type) return Signal_Error
   is
      Result : Signal_Error;
      Len    : constant Natural :=
        Natural'Min (Message'Length, Result.Message'Length);
   begin
      Result.Message (1 .. Len) :=
        Message (Message'First .. Message'First + Len - 1);
      Result.Length := Len;
      Result.Signal := Signal;
      return Result;
   end Make_Error;

   --  Protected state for signal handling
   --  This replaces the task-based implementation with protected objects only
   protected body Signal_State is

      procedure Set_Handler (Handler : Signal_Handler_With_Context) is
      begin
         Signal_State.Handler := Handler;
      end Set_Handler;

      function Get_Handler return Signal_Handler_With_Context
      is (Handler);

      procedure Signal_Received (Signal : Signal_Type) is
      begin
         Last_Sig := Signal;
         Has_Signal := True;

         --  Invoke handler if set
         if Handler /= null then
            declare
               Cancel_Flag : Boolean;
            begin
               Handler (Signal, Cancel_Flag);
               if Cancel_Flag then
                  Cancelled := True;
               end if;
            end;
         end if;
      end Signal_Received;

      function Last_Signal return Signal_Type
      is (Last_Sig);

      procedure Set_Cancelled (Value : Boolean) is
      begin
         Cancelled := Value;
      end Set_Cancelled;

      function Is_Cancelled return Boolean
      is (Cancelled);

      entry Wait_For_Signal (Signal : out Signal_Type) when Has_Signal is
      begin
         Signal := Last_Sig;
         Has_Signal := False;
      end Wait_For_Signal;

   end Signal_State;

   --  Constructor
   function Create return System_Signals is
     (State              => new Signal_State,
      Task_Handler       => null,  --  No longer used
      Interrupt_Handlers => null,  --  Replaced by AbohLib handler
      Installed          => False,
      AbohLib_Handler    => null,
      Cancellation       => new Cancellation_Source_Type);

   --  Install signal handler
   function Install
     (Self : in out System_Signals; Handler : Signal_Handler_With_Context)
      return Result
   is
   begin
      if Self.Installed then
         return Signal_Result.Err
           (Make_Error ("Signal handlers already installed", Interrupt));
      end if;

      --  Store the handler in our state
      Self.State.Set_Handler (Handler);

      --  Create AbohLib POSIX signal handler
      Self.AbohLib_Handler :=
        new AbohLib.Infrastructure.OS.Signal_Handler.POSIX.POSIX_Signal_Handler'
          (AbohLib.Infrastructure.OS.Signal_Handler.POSIX.Create);

      --  Install handlers for SIGINT and SIGTERM
      Self.AbohLib_Handler.Install_Multiple_Handlers
        (Cancellation => Self.Cancellation.all'Unchecked_Access,
         Signals      => [SIGINT, SIGTERM]);

      Self.Installed := True;
      return Signal_Result.Ok (True);

   exception
      when others =>
         return Signal_Result.Err
           (Make_Error ("Failed to install signal handlers", Interrupt));
   end Install;

   --  Legacy install (not used, kept for API compatibility)
   procedure Install (Self : in out System_Signals; Handler : Signal_Handler)
   is
      pragma Unreferenced (Handler);
   begin
      raise Program_Error with
        "Legacy Install procedure not supported with AbohLib backend. " &
        "Use Install with Signal_Handler_With_Context instead.";
   end Install;

   --  Uninstall signal handler
   function Uninstall (Self : in out System_Signals) return Result is
   begin
      if not Self.Installed then
         return Signal_Result.Ok (True);  --  Already uninstalled
      end if;

      --  Remove AbohLib signal handlers
      if Self.AbohLib_Handler /= null then
         Self.AbohLib_Handler.Remove_Handlers;
      end if;

      Self.Installed := False;
      return Signal_Result.Ok (True);

   exception
      when others =>
         return Signal_Result.Err
           (Make_Error ("Failed to uninstall signal handlers", Interrupt));
   end Uninstall;

   --  Legacy uninstall
   procedure Uninstall (Self : in out System_Signals) is
      Ignore : constant Hybrid.Bootstrap.Signals.Result := Self.Uninstall;
      pragma Unreferenced (Ignore);
   begin
      null;  --  Ignore result for legacy API
   end Uninstall;

   --  Check if cancellation was requested
   function Is_Cancelled (Self : System_Signals) return Boolean is
   begin
      --  Check both our state and the AbohLib cancellation source
      if Self.State /= null and then Self.State.Is_Cancelled then
         return True;
      end if;

      if Self.Cancellation /= null
        and then Self.Cancellation.Is_Cancelled
      then
         return True;
      end if;

      return False;
   end Is_Cancelled;

   --  Request cancellation
   procedure Request_Cancellation (Self : in out System_Signals) is
   begin
      if Self.State /= null then
         Self.State.Set_Cancelled (True);
      end if;

      if Self.Cancellation /= null then
         Self.Cancellation.Request_Cancellation ("User requested cancellation");
      end if;
   end Request_Cancellation;

   --  Get last signal received
   function Last_Signal (Self : System_Signals) return Signal_Type is
   begin
      if Self.State /= null then
         return Self.State.Last_Signal;
      end if;

      --  Check AbohLib handler
      if Self.AbohLib_Handler /= null then
         declare
            AbohLib_Sig : constant AbohLib.Infrastructure.OS.Signal_Handler.Signal_Type :=
              Self.AbohLib_Handler.Last_Signal;
         begin
            return To_Hybrid_Signal (AbohLib_Sig);
         end;
      end if;

      return Interrupt;  --  Default
   end Last_Signal;

   --  =========================================================================
   --  DEPRECATED: Stubs for old task-based implementation
   --  =========================================================================
   --  These are kept for API compatibility but are no longer used

   task body Signal_Handler_Task is
   begin
      accept Start;
      select
         accept Stop;
      or
         accept Get_Status (Success : out Boolean; Error : out Signal_Error) do
            Success := True;
            Error := Make_Error ("Task deprecated", Interrupt);
         end Get_Status;
      or
         terminate;
      end select;
      --  Immediately terminate, no longer needed
   end Signal_Handler_Task;

   protected body Interrupt_Handler is
      procedure Handle_SIGINT is
      begin
         null;  --  No longer used
      end Handle_SIGINT;

      procedure Handle_SIGTERM is
      begin
         null;  --  No longer used
      end Handle_SIGTERM;

      procedure Handle_SIGKILL is
      begin
         null;  --  No longer used
      end Handle_SIGKILL;

      procedure Handle_SIGUSR1 is
      begin
         null;  --  No longer used
      end Handle_SIGUSR1;

      procedure Handle_SIGUSR2 is
      begin
         null;  --  No longer used
      end Handle_SIGUSR2;

      procedure Handle_SIGALRM is
      begin
         null;  --  No longer used
      end Handle_SIGALRM;

      procedure Set_State (State : Signal_State_Access) is
      begin
         Signal_State := State;
      end Set_State;
   end Interrupt_Handler;

end Hybrid.Bootstrap.Signals;
