pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Application.Result.Result_Port - Result Port Generic Signature
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Generic signature for Result<T,E> port at Application layer.
--    This prevents Presentation from directly importing Domain types.
--
--  Architecture Note:
--    This is a facade matching Domain.Foundation.Ports.Result_Port signature
--    but at the Application layer, maintaining the dependency rule:
--    Presentation → Application → Domain (not Presentation → Domain)
--
--  Usage:
--    In Presentation layer, instantiate this generic:
--      with package R is new Hybrid.Application.Result.Result_Port
--        (T => String_Type, E => Error_Type, ...);
--
--  See Also:
--    Hybrid.Domain.Foundation.Ports.Result_Port - underlying implementation
--    Hybrid.Presentation.CLI - consumer of this facade
--  ==========================================================================

generic
   type T (<>) is private;
   type E is private;
   type Result is private;
   with function Ok (V : T) return Result is <>;
   with function Err (E_Val : E) return Result is <>;
   with function Is_Ok (R : Result) return Boolean is <>;
   with function Is_Err (R : Result) return Boolean is <>;
   with function Value (R : Result) return T is <>;
   with function Error (R : Result) return E is <>;
   with function Unwrap_Or (R : Result; Default : T) return T is <>;
package Hybrid.Application.Result.Result_Port is
   --  Signature-only generic package
   --  All operations provided via generic formal parameters
   --  Empty body provided for library builds
   --
   --  Note: Formal parameters are intentionally unused in this facade package,
   --  but are used by consumers when instantiated. Compiler warnings about
   --  unreferenced formals are expected and safe for signature-only generics.
end Hybrid.Application.Result.Result_Port;
