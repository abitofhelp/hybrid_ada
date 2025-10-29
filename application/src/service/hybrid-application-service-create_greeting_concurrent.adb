pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Service.Create_Greeting_Concurrent - Concurrent greeting
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Notes:
--    - Ada 2022; aspects preferred over pragmas when available.
--    - 78-character separators enforced.
--    - Demonstrates Ada concurrency: tasks for parallel processing.
--  ==========================================================================

package body Hybrid.Application.Service.Create_Greeting_Concurrent is
   use Ada.Strings.Unbounded;

   package body API is

      function Run (Raw_Name : String) return R.Result is
         --  Task for creating Person_Name
         task type Name_Creator is
            entry Start (Input : String);
            entry Get_Result (Res : out R_Name.Result);
         end Name_Creator;

         task body Name_Creator is
            Name_Input  : Unbounded_String;
            Name_Result : R_Name.Result;
         begin
            accept Start (Input : String) do
               Name_Input := To_Unbounded_String (Input);
            end Start;

            Name_Result := Name_Ops.Create (To_String (Name_Input));

            accept Get_Result (Res : out R_Name.Result) do
               Res := Name_Result;
            end Get_Result;
         end Name_Creator;

         --  Task for creating greeting message
         task type Message_Creator is
            entry Create_And_Return
              (Name : Hybrid.Domain.Value.Person_Name.Person_Name;
               Res  : out R.Result);
         end Message_Creator;

         task body Message_Creator is
         begin
            accept Create_And_Return
              (Name : Hybrid.Domain.Value.Person_Name.Person_Name;
               Res  : out R.Result)
            do
               declare
                  Msg : constant Unbounded_String :=
                    To_Unbounded_String
                      ("Hello, "
                       & Hybrid.Domain.Value.Person_Name.To_String (Name)
                       & "!");
               begin
                  Res := R.Ok (Msg);
               end;
            end Create_And_Return;
         end Message_Creator;

         --  Task instances
         Name_Task : Name_Creator;
         Msg_Task  : Message_Creator;

         --  Results
         Name_Result  : R_Name.Result;
         Final_Result : R.Result;
      begin
         --  Start name creation task
         Name_Task.Start (Raw_Name);

         --  Wait for name creation to complete
         Name_Task.Get_Result (Name_Result);

         --  Check if name creation succeeded
         if R_Name.Is_Err (Name_Result) then
            return R.Err (Map_Error (R_Name.Error (Name_Result)));
         end if;

         --  Create greeting message concurrently with validated name
         Msg_Task.Create_And_Return (R_Name.Value (Name_Result), Final_Result);

         --  Send the message via output port
         if R.Is_Ok (Final_Result) then
            declare
               Msg         : constant Unbounded_String :=
                 R.Value (Final_Result);
               Send_Result : constant R.Result :=
                 Output_Port.Send (Msg);
            begin
               if R.Is_Err (Send_Result) then
                  return R.Err (R.Error (Send_Result));
               end if;
            end;
         end if;

         return Final_Result;
      end Run;

   end API;

end Hybrid.Application.Service.Create_Greeting_Concurrent;
