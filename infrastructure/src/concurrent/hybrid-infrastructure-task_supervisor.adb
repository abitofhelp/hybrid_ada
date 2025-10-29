pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Task_Supervisor - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Infrastructure.Task_Supervisor is

   protected body Supervisor is

      procedure Register (Id : Task_Id; Name : String) is
      begin
         if Task_Count < Tasks'Last then
            Task_Count := Task_Count + 1;
            Tasks (Task_Count) :=
              (Id => Id, Name => [others => ' '], Name_Length => Name'Length);
            Tasks (Task_Count).Name (1 .. Name'Length) := Name;
         end if;
      end Register;

      procedure Unregister (Id : Task_Id) is
      begin
         for I in 1 .. Task_Count loop
            if Tasks (I).Id = Id then
               -- Shift remaining tasks
               for J in I .. Task_Count - 1 loop
                  Tasks (J) := Tasks (J + 1);
               end loop;
               Task_Count := Task_Count - 1;
               exit;
            end if;
         end loop;
      end Unregister;

      procedure Request_Shutdown is
      begin
         Shutdown_Requested := True;
      end Request_Shutdown;

      function Is_Shutdown_Requested return Boolean
      is (Shutdown_Requested);

      entry Wait_For_All_Tasks when Task_Count = 0 is
      begin
         null;
      end Wait_For_All_Tasks;

      function Active_Task_Count return Natural
      is (Task_Count);

   end Supervisor;

   procedure Register (Reg : in out Task_Registration; Name : String) is
   begin
      if not Reg.Registered then
         Reg.Task_Id := Current_Task;
         Global_Supervisor.Register (Reg.Task_Id, Name);
         Reg.Registered := True;
      end if;
   end Register;

   overriding
   procedure Finalize (Object : in out Task_Registration) is
   begin
      if Object.Registered then
         Global_Supervisor.Unregister (Object.Task_Id);
         Object.Registered := False;
      end if;
   end Finalize;

   procedure Graceful_Shutdown_Loop is
      Check_Interval : constant Duration := 0.1;
   begin
      loop
         exit when Global_Supervisor.Is_Shutdown_Requested;
         delay Check_Interval;
      end loop;
      Shutdown_Handler;
   end Graceful_Shutdown_Loop;

end Hybrid.Infrastructure.Task_Supervisor;
