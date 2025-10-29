pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Task_Supervisor
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Task supervision pattern for managing Ada tasks. Provides lifecycle management and graceful shutdown coordination.
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

with Ada.Task_Identification;
with Ada.Finalization;

package Hybrid.Infrastructure.Task_Supervisor is

   use Ada.Task_Identification;

   -- Task registration info
   type Task_Info is record
      Id          : Task_Id;
      Name        : String (1 .. 50);
      Name_Length : Natural;
   end record;

   -- Array type for tasks
   type Task_Array is array (1 .. 100) of Task_Info;

   -- Task supervisor protected object
   protected type Supervisor is
      -- Register a task
      procedure Register (Id : Task_Id; Name : String);

      -- Unregister a task
      procedure Unregister (Id : Task_Id);

      -- Request shutdown of all tasks
      procedure Request_Shutdown;

      -- Check if shutdown was requested
      function Is_Shutdown_Requested return Boolean;

      -- Wait for all tasks to complete
      entry Wait_For_All_Tasks;

      -- Get count of active tasks
      function Active_Task_Count return Natural;

   private
      Tasks : Task_Array;
      Task_Count : Natural := 0;
      Shutdown_Requested : Boolean := False;
   end Supervisor;

   -- Global supervisor instance
   Global_Supervisor : Supervisor;

   -- RAII task registration
   type Task_Registration is new Ada.Finalization.Limited_Controlled
   with record
      Registered : Boolean := False;
      Task_Id    : Ada.Task_Identification.Task_Id;
   end record;

   -- Register current task
   procedure Register (Reg : in out Task_Registration; Name : String);

   -- Lifecycle
   overriding
   procedure Finalize (Object : in out Task_Registration);

   -- Graceful shutdown helper
   generic
      with procedure Shutdown_Handler;
   procedure Graceful_Shutdown_Loop;

end Hybrid.Infrastructure.Task_Supervisor;
