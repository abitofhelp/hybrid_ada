pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Infrastructure.Error_Transform
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines error types for the Infrastructure layer including
--    Error_Context, Result_Type, Result_Type. Provides type-safe error
--    handling.
--
--  Usage:
--    Error types are used with Result monad for type-safe error handling:
--       Result := Create (Input);
--       if not Is_Ok (Result) then
--          Handle_Error (Get_Err (Result));
--       end if;
--
--  Design Notes:
--    Infrastructure layer - implements technical capabilities
--    Generic package - requires instantiation with specific types
--
--  See Also:
--    Hybrid.Application.Error - dependency
--  ==========================================================================

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Hybrid.Application.Error;

package Hybrid.Infrastructure.Error_Transform is

   -- Error context information
   type Error_Context is record
      Component : Unbounded_String;
      Operation : Unbounded_String;
      Details   : Unbounded_String;
      Timestamp : Ada.Calendar.Time;
   end record;

   -- NOTE: Functions returning abstract types have been removed.
   -- Future implementations should use concrete error types or
   -- class-wide return types for error transformation.
   --
   -- Educational Note: Ada does not allow functions to return abstract types
   -- directly. Use class-wide types (Domain_Error'Class) for polymorphic
   -- returns, or return specific concrete error types.

   -- Safe exception handler with context
   --
   -- Educational Note: These generic utilities provide exception-to-Result
   -- transformation at infrastructure boundaries. Use when wrapping external
   -- APIs that may raise exceptions into the functional Result pattern.
   --
   -- Example:
   --    function Safe_File_Read is new Safe_Execute
   --      (Result_Type => File_Result.Result,
   --       Error_Result => File_Result.Err);
   --
   --    Result := Safe_File_Read
   --      (Read_File'Access,
   --       Context => (Component => "FileSystem", Operation => "Read", ...));
   generic
      type Result_Type is private;
      with
        function Error_Result
          (E : Application.Error.Application_Error) return Result_Type;
   function Safe_Execute
     (Operation : access function return Result_Type; Context : Error_Context)
      return Result_Type;

   -- Safe procedure wrapper
   generic
      type Result_Type is private;
      with function Success_Result return Result_Type;
      with
        function Error_Result
          (E : Application.Error.Application_Error) return Result_Type;
   function Safe_Procedure
     (Operation : access procedure; Context : Error_Context)
      return Result_Type;

   -- Exception classification
   type Exception_Category is
     (Storage_Exception,
      Program_Exception,
      Constraint_Exception,
      IO_Exception,
      Tasking_Exception,
      Unknown_Exception);

   function Classify_Exception
     (E : Ada.Exceptions.Exception_Occurrence) return Exception_Category;

   -- Error chain builder
   type Error_Chain is tagged private;

   function Create_Chain (Initial : Error_Context) return Error_Chain;

   procedure Add_Context
     (Chain     : in out Error_Chain;
      Component : String;
      Operation : String;
      Details   : String := "");

   function Build_Error
     (Chain : Error_Chain; E : Ada.Exceptions.Exception_Occurrence)
      return Application.Error.Application_Error;

private

   type Context_Node;
   type Context_Node_Access is access Context_Node;

   type Context_Node is record
      Context : Error_Context;
      Next    : Context_Node_Access;
   end record;

   type Error_Chain is tagged record
      Head : Context_Node_Access;
      Tail : Context_Node_Access;
   end record;

end Hybrid.Infrastructure.Error_Transform;
