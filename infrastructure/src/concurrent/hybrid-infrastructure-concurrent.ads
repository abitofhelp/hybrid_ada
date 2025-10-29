pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Concurrent
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Concurrency utilities and protected types. Provides thread-safe primitives for concurrent operations.
--
--  Usage:
--    See package specification for detailed usage examples.
--
--  Design Notes:
--    Infrastructure layer - implements technical capabilities
--    Generic package - requires instantiation with specific types
--    Concurrent design using Ada tasks/protected objects
--
--  See Also:
--    Other Infrastructure layer packages
--  ==========================================================================

with Ada.Finalization;

package Hybrid.Infrastructure.Concurrent is

   -- Task pool size configuration
   Default_Worker_Count : constant := 4;

   -- Message priority levels
   type Priority_Level is (Low, Normal, High, Critical);

   -- Generic worker task type
   generic
      type Work_Item is private;
   package Worker_Pool is

      -- Named array types for protected type components
      --
      -- Educational Note: Ada requires all types in protected objects to be
      -- named types, not anonymous arrays. This ensures proper initialization
      -- and component visibility.
      type Work_Item_Queue_Type is array (Priority_Level) of Work_Item;
      type Priority_Count_Type is array (Priority_Level) of Natural;

      -- Queue for work items
      protected type Work_Queue is
         entry Put (Item : Work_Item; Priority : Priority_Level := Normal);
         entry Get (Item : out Work_Item);
         procedure Shutdown;
         function Is_Shutting_Down return Boolean;
      private
         -- Priority queues
         Queue : Work_Item_Queue_Type;
         Count : Priority_Count_Type := [others => 0];
         Shutting_Down : Boolean := False;
      end Work_Queue;

      -- Worker task with discriminant for queue access
      --
      -- Educational Note: Task entries cannot have access parameters, so we use
      -- a discriminant to pass the queue reference at task creation time.
      task type Worker (Queue : access Work_Queue) is
         entry Stop;
      end Worker;

      type Worker_Access is access Worker;
      type Worker_Array is array (Positive range <>) of Worker_Access;
      type Worker_Array_Access is access Worker_Array;

      -- Task pool manager
      type Pool_Manager is new Ada.Finalization.Limited_Controlled with record
         Queue   : aliased Work_Queue;
         Workers : Worker_Array_Access;
      end record;

      overriding
      procedure Initialize (Object : in out Pool_Manager);
      overriding
      procedure Finalize (Object : in out Pool_Manager);

      procedure Submit
        (Pool     : in out Pool_Manager;
         Item     : Work_Item;
         Priority : Priority_Level := Normal);
      procedure Shutdown (Pool : in out Pool_Manager);

   end Worker_Pool;

   -- Handler types for Message_Dispatcher
   type Handler_Procedure_Access is access procedure (Message : String);
   type Handler_Array_Type is array (1 .. 10) of Handler_Procedure_Access;

   -- Concurrent message dispatcher for async I/O
   protected type Message_Dispatcher is
      procedure Register_Handler (Handler : Handler_Procedure_Access);
      procedure Dispatch (Message : String);
      procedure Clear_Handlers;
   private
      Handlers : Handler_Array_Type := [others => null];
      Handler_Count : Natural := 0;
   end Message_Dispatcher;

   -- Task-safe counter
   protected type Counter is
      procedure Increment;
      procedure Decrement;
      function Value return Natural;
      entry Wait_For_Zero;
   private
      Count : Natural := 0;
   end Counter;

end Hybrid.Infrastructure.Concurrent;
