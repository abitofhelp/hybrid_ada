pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Bootstrap.Signals
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Signal handling for graceful shutdown. Handles SIGTERM and SIGINT for clean resource cleanup.
--
--  Usage:
--    See package specification for detailed usage examples.
--
--  Design Notes:
--    Uses Ada 2022 contracts (Pre/Post) for design-by-contract
--
--  See Also:
--    Hybrid.Domain.Model.Result - dependency
--  ==========================================================================

with Hybrid.Infrastructure.Adapter.Functional.Result_API;
with AbohLib.Infrastructure.OS.Signal_Handler;
with AbohLib.Infrastructure.Concurrent.Cancellation_Source;

package Hybrid.Bootstrap.Signals is

   -- Signal types
   type Signal_Type is (Interrupt, Term, Kill, User1, User2, Alarm);

   -- Signal error types
   type Signal_Error is record
      Message : String (1 .. 100);
      Length  : Natural;
      Signal  : Signal_Type;
   end record;

   -- Make a signal error
   function Make_Error
     (Message : String; Signal : Signal_Type) return Signal_Error;

   -- Result types for signal operations
   -- Bootstrap can directly use adapters (infrastructure concern)
   package Signal_Result_API is new
     Hybrid.Infrastructure.Adapter.Functional.Result_API
       (T => Boolean,
        E => Signal_Error);

   package Signal_Result renames Signal_Result_API.FR;
   subtype Result is Signal_Result.Result;

   -- Signal handler with cancellation support
   type Signal_Handler_With_Context is
     access procedure (Signal : Signal_Type; Cancelled : out Boolean);

   -- Legacy support
   type Signal_Handler is access procedure;

   -- Enhanced signal handler
   type System_Signals is tagged private;

   -- Constructor
   function Create return System_Signals;

   -- Install signal handler with error handling
   function Install
     (Self : in out System_Signals; Handler : Signal_Handler_With_Context)
      return Result;

   -- Legacy install (converts to new handler)
   procedure Install (Self : in out System_Signals; Handler : Signal_Handler)
   with Pre => Handler /= null;

   -- Uninstall signal handler
   function Uninstall (Self : in out System_Signals) return Result;

   -- Legacy uninstall
   procedure Uninstall (Self : in out System_Signals);

   -- Check if cancellation was requested
   function Is_Cancelled (Self : System_Signals) return Boolean;

   -- Request cancellation of all operations
   procedure Request_Cancellation (Self : in out System_Signals);

   -- Get last signal received
   function Last_Signal (Self : System_Signals) return Signal_Type;

private

   -- Protected object for thread-safe signal handling
   protected type Signal_State is
      procedure Set_Handler (Handler : Signal_Handler_With_Context);
      function Get_Handler return Signal_Handler_With_Context;
      procedure Signal_Received (Signal : Signal_Type);
      function Last_Signal return Signal_Type;
      procedure Set_Cancelled (Value : Boolean);
      function Is_Cancelled return Boolean;
      entry Wait_For_Signal (Signal : out Signal_Type);
   private
      Handler : Signal_Handler_With_Context;
      Last_Sig : Signal_Type := Interrupt;
      Has_Signal : Boolean := False;
      Cancelled : Boolean := False;
   end Signal_State;

   type Signal_State_Access is access Signal_State;

   -- Signal handler task with error recovery
   task type Signal_Handler_Task (State : Signal_State_Access) is
      entry Start;
      entry Stop;
      entry Get_Status (Success : out Boolean; Error : out Signal_Error);
   end Signal_Handler_Task;

   type Signal_Handler_Task_Access is access Signal_Handler_Task;

   -- Interrupt handlers for each signal type
   protected type Interrupt_Handler is
      procedure Handle_SIGINT;
      procedure Handle_SIGTERM;
      procedure Handle_SIGKILL;
      procedure Handle_SIGUSR1;
      procedure Handle_SIGUSR2;
      procedure Handle_SIGALRM;
      procedure Set_State (State : Signal_State_Access);
   private
      Signal_State : Signal_State_Access;
   end Interrupt_Handler;

   type Interrupt_Handler_Access is access Interrupt_Handler;

   --  Forward declarations for AbohLib types
   type AbohLib_Signal_Handler_Access is access all
     AbohLib.Infrastructure.OS.Signal_Handler.Signal_Handler_Interface'Class;
   type AbohLib_Cancellation_Access is access all
     AbohLib.Infrastructure.Concurrent.Cancellation_Source.Cancellation_Source_Type;

   type System_Signals is tagged record
      State              : Signal_State_Access;
      Task_Handler       : Signal_Handler_Task_Access;  --  Deprecated
      Interrupt_Handlers : Interrupt_Handler_Access;    --  Deprecated
      Installed          : Boolean := False;
      AbohLib_Handler    : AbohLib_Signal_Handler_Access;
      Cancellation       : AbohLib_Cancellation_Access;
   end record;

end Hybrid.Bootstrap.Signals;
