pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Error_Transform - Implementation
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  ==========================================================================

package body Hybrid.Infrastructure.Error_Transform is

   -- Safe exception boundary wrappers for converting exceptions to Results
   -- These provide exception-to-Result transformation at infrastructure boundaries

   function Safe_Execute
     (Operation : access function return Result_Type; Context : Error_Context)
      return Result_Type
   is
   begin
      return Operation.all;
   exception
      when E : others =>
         declare
            App_Err : constant Application.Error.Application_Error :=
              Application.Error.New_Application_Error
                (Use_Case  => To_String (Context.Component),
                 Operation => To_String (Context.Operation),
                 Message   =>
                   To_String (Context.Details) & ": " &
                   Ada.Exceptions.Exception_Message (E));
         begin
            return Error_Result (App_Err);
         end;
   end Safe_Execute;

   function Safe_Procedure
     (Operation : access procedure; Context : Error_Context) return Result_Type
   is
   begin
      Operation.all;
      return Success_Result;
   exception
      when E : others =>
         declare
            App_Err : constant Application.Error.Application_Error :=
              Application.Error.New_Application_Error
                (Use_Case  => To_String (Context.Component),
                 Operation => To_String (Context.Operation),
                 Message   =>
                   To_String (Context.Details) & ": " &
                   Ada.Exceptions.Exception_Message (E));
         begin
            return Error_Result (App_Err);
         end;
   end Safe_Procedure;

   function Classify_Exception
     (E : Ada.Exceptions.Exception_Occurrence) return Exception_Category
   is
      Name : constant String := Ada.Exceptions.Exception_Name (E);
   begin
      if Name = "STORAGE_ERROR" then
         return Storage_Exception;
      elsif Name = "PROGRAM_ERROR" then
         return Program_Exception;
      elsif Name = "CONSTRAINT_ERROR" then
         return Constraint_Exception;
      elsif Name = "ADA.TEXT_IO.DEVICE_ERROR"
        or Name = "ADA.TEXT_IO.USE_ERROR"
        or Name = "ADA.TEXT_IO.DATA_ERROR"
      then
         return IO_Exception;
      elsif Name = "TASKING_ERROR" then
         return Tasking_Exception;
      else
         return Unknown_Exception;
      end if;
   end Classify_Exception;

   function Create_Chain (Initial : Error_Context) return Error_Chain is
      Node : constant Context_Node_Access :=
        new Context_Node'(Context => Initial, Next => null);
   begin
      return (Head => Node, Tail => Node);
   end Create_Chain;

   procedure Add_Context
     (Chain     : in out Error_Chain;
      Component : String;
      Operation : String;
      Details   : String := "")
   is
      Node : constant Context_Node_Access :=
        new Context_Node'
          (Context =>
             (Component => To_Unbounded_String (Component),
              Operation => To_Unbounded_String (Operation),
              Details   => To_Unbounded_String (Details),
              Timestamp => Ada.Calendar.Clock),
           Next    => null);
   begin
      if Chain.Tail /= null then
         Chain.Tail.Next := Node;
         Chain.Tail := Node;
      else
         Chain.Head := Node;
         Chain.Tail := Node;
      end if;
   end Add_Context;

   function Build_Error
     (Chain : Error_Chain; E : Ada.Exceptions.Exception_Occurrence)
      return Application.Error.Application_Error
   is

      Message : Unbounded_String := To_Unbounded_String ("");
      Current : Context_Node_Access := Chain.Head;
   begin
      -- Build error chain message
      while Current /= null loop
         Append
           (Message,
            "["
            & To_String (Current.Context.Component)
            & "."
            & To_String (Current.Context.Operation)
            & "]");
         if Current.Next /= null then
            Append (Message, " -> ");
         end if;
         Current := Current.Next;
      end loop;

      Append (Message, ": " & Ada.Exceptions.Exception_Message (E));

      -- Get the most recent context for the main error
      if Chain.Tail /= null then
         return
           Application.Error.New_Application_Error
             (Use_Case  => To_String (Chain.Tail.Context.Component),
              Operation => To_String (Chain.Tail.Context.Operation),
              Message   => To_String (Message));
      else
         return
           Application.Error.New_Application_Error
             (Use_Case  => "Unknown",
              Operation => "Unknown",
              Message   => Ada.Exceptions.Exception_Message (E));
      end if;
   end Build_Error;

end Hybrid.Infrastructure.Error_Transform;
