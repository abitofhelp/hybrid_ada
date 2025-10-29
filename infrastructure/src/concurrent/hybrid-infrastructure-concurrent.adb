pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Concurrent - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Infrastructure.Concurrent is

   package body Worker_Pool is

      protected body Work_Queue is

         entry Put (Item : Work_Item; Priority : Priority_Level := Normal)
           when not Shutting_Down
         is
         begin
            Queue (Priority) := Item;
            Count (Priority) := Count (Priority) + 1;
         end Put;

         entry Get (Item : out Work_Item)
           when(for some P in Priority_Level => Count (P) > 0) or Shutting_Down
         is
         begin
            if Shutting_Down then
               raise Program_Error with "Queue is shutting down";
            end if;

            -- Get highest priority item
            for P in reverse Priority_Level'Range loop
               if Count (P) > 0 then
                  Item := Queue (P);
                  Count (P) := Count (P) - 1;
                  return;
               end if;
            end loop;
         end Get;

         procedure Shutdown is
         begin
            Shutting_Down := True;
         end Shutdown;

         function Is_Shutting_Down return Boolean
         is (Shutting_Down);

      end Work_Queue;

      task body Worker is
         Item    : Work_Item;
         Running : Boolean := True;
      begin
         while Running loop
            -- Check for stop request using timed select
            select
               accept Stop do
                  Running := False;
               end Stop;
            or
               -- Use delay to allow periodic checking of work queue
               delay 0.01;  -- Short delay to check work queue frequently

               -- Check if shutting down (using discriminant Queue)
               if Worker.Queue.Is_Shutting_Down then
                  Running := False;
               else
                  -- Try to get work with a timeout (using discriminant Queue)
                  select
                     Worker.Queue.Get (Item);
                     -- Process work item (application specific)
                     null; -- Placeholder for actual work processing
                  or
                     delay 0.1;  -- Timeout if no work available
                     null; -- Continue to next iteration
                  end select;
               end if;
            end select;
         end loop;
      exception
         when others =>
            null; -- Graceful shutdown on exceptions
      end Worker;

      overriding
      procedure Initialize (Object : in out Pool_Manager) is
      begin
         Object.Workers := new Worker_Array (1 .. Default_Worker_Count);
         -- Create workers with discriminant pointing to the pool's queue
         --
         -- Educational Note: We use 'Unchecked_Access here because the normal
         -- accessibility check fails (Object is a parameter). However, this is
         -- safe because Pool_Manager is Limited_Controlled and owns both the
         -- Queue and the Workers. Finalize ensures workers are destroyed before
         -- the queue, so the reference remains valid for the workers' lifetime.
         for I in Object.Workers'Range loop
            Object.Workers (I) :=
              new Worker (Queue => Object.Queue'Unchecked_Access);
         end loop;
      end Initialize;

      overriding
      procedure Finalize (Object : in out Pool_Manager) is
      begin
         if Object.Workers /= null then
            Object.Queue.Shutdown;
            for I in Object.Workers'Range loop
               begin
                  if Object.Workers (I) /= null then
                     Object.Workers (I).Stop;
                  end if;
               exception
                  when Tasking_Error =>
                     null; -- Task already terminated
               end;
            end loop;
         end if;
      end Finalize;

      procedure Submit
        (Pool     : in out Pool_Manager;
         Item     : Work_Item;
         Priority : Priority_Level := Normal) is
      begin
         Pool.Queue.Put (Item, Priority);
      end Submit;

      procedure Shutdown (Pool : in out Pool_Manager) is
      begin
         Pool.Queue.Shutdown;
      end Shutdown;

   end Worker_Pool;

   protected body Message_Dispatcher is

      procedure Register_Handler (Handler : Handler_Procedure_Access) is
      begin
         if Handler_Count < Handlers'Length then
            Handler_Count := Handler_Count + 1;
            Handlers (Handler_Count) := Handler;
         else
            raise Program_Error with "Handler array full";
         end if;
      end Register_Handler;

      procedure Dispatch (Message : String) is
      begin
         for I in 1 .. Handler_Count loop
            if Handlers (I) /= null then
               Handlers (I).all (Message);
            end if;
         end loop;
      end Dispatch;

      procedure Clear_Handlers is
      begin
         Handler_Count := 0;
         Handlers := [others => null];
      end Clear_Handlers;

   end Message_Dispatcher;

   protected body Counter is

      procedure Increment is
      begin
         Count := Count + 1;
      end Increment;

      procedure Decrement is
      begin
         if Count > 0 then
            Count := Count - 1;
         end if;
      end Decrement;

      function Value return Natural
      is (Count);

      entry Wait_For_Zero when Count = 0 is
      begin
         null;
      end Wait_For_Zero;

   end Counter;

end Hybrid.Infrastructure.Concurrent;
