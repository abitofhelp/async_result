-- =============================================================================
-- Ada Async_Result Library - Prelude (Common Instantiations)
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Pre-instantiated Result types for common use cases
-- Import this to get started quickly without generic instantiation
-- =============================================================================

with Ada.Strings.Unbounded;
with Async_Result.Core;
with Async_Result.Simple;
with Async_Result.Async;
with Async_Result.Pool;
with Async_Result.Metrics;

package Async_Result.Prelude is
   pragma Elaborate_Body;

   use Ada.Strings.Unbounded;

   -- ==========================================================================
   -- Common Error Types
   -- ==========================================================================
   
   type Error_Kind is (
      None,
      Invalid_Argument,
      Out_Of_Bounds,
      Null_Reference,
      Type_Error,
      IO_Error,
      Network_Error,
      Timeout_Error,
      Permission_Error,
      Resource_Error,
      Parse_Error,
      State_Error,
      Concurrency_Error,
      Unknown_Error
   );
   
   type Error_Info is record
      Kind : Error_Kind := None;
      Message : Unbounded_String := Null_Unbounded_String;
      Code : Integer := 0;
   end record;

   -- ==========================================================================
   -- Integer Results
   -- ==========================================================================
   
   -- Helper functions for Integer
   function Copy_Integer (I : Integer) return Integer is (I);
   function Default_Integer return Integer is (0);
   
   -- Helper functions for Error_Info
   function Copy_Error_Info (E : Error_Info) return Error_Info is (E);
   function Default_Error_Info return Error_Info is 
      ((Kind => None, Message => Null_Unbounded_String, Code => 0));
   
   -- Core Integer Result
   package Integer_Result is new Async_Result.Core (
      Value_Type => Integer,
      Error_Type => Error_Info,
      Copy_Value => Copy_Integer,
      Copy_Error => Copy_Error_Info,
      Default_Value => Default_Integer,
      Default_Error => Default_Error_Info
   );
   
   -- Simple Integer Result
   package Simple_Integer is new Async_Result.Simple (
      Value_Type => Integer,
      Error_Type => Error_Info
   );
   
   -- Async Integer operations
   package Async_Integer is new Async_Result.Async (
      Core => Integer_Result
   );

   -- ==========================================================================
   -- String Results
   -- ==========================================================================
   
   -- Helper functions for String
   function Copy_String (S : Unbounded_String) return Unbounded_String is (S);
   function Default_String return Unbounded_String is (Null_Unbounded_String);
   
   -- Core String Result
   package String_Result is new Async_Result.Core (
      Value_Type => Unbounded_String,
      Error_Type => Error_Info,
      Copy_Value => Copy_String,
      Copy_Error => Copy_Error_Info,
      Default_Value => Default_String,
      Default_Error => Default_Error_Info
   );
   
   -- Simple String Result  
   package Simple_String is new Async_Result.Simple (
      Value_Type => Unbounded_String,
      Error_Type => Error_Info
   );
   
   -- Async String operations
   package Async_String is new Async_Result.Async (
      Core => String_Result
   );

   -- ==========================================================================
   -- Float Results
   -- ==========================================================================
   
   -- Helper functions for Float
   function Copy_Float (F : Float) return Float is (F);
   function Default_Float return Float is (0.0);
   
   -- Core Float Result
   package Float_Result is new Async_Result.Core (
      Value_Type => Float,
      Error_Type => Error_Info,
      Copy_Value => Copy_Float,
      Copy_Error => Copy_Error_Info,
      Default_Value => Default_Float,
      Default_Error => Default_Error_Info
   );
   
   -- Simple Float Result
   package Simple_Float is new Async_Result.Simple (
      Value_Type => Float,
      Error_Type => Error_Info
   );
   
   -- Async Float operations
   package Async_Float is new Async_Result.Async (
      Core => Float_Result
   );

   -- ==========================================================================
   -- Boolean Results
   -- ==========================================================================
   
   -- Helper functions for Boolean
   function Copy_Boolean (B : Boolean) return Boolean is (B);
   function Default_Boolean return Boolean is (False);
   
   -- Core Boolean Result
   package Boolean_Result is new Async_Result.Core (
      Value_Type => Boolean,
      Error_Type => Error_Info,
      Copy_Value => Copy_Boolean,
      Copy_Error => Copy_Error_Info,
      Default_Value => Default_Boolean,
      Default_Error => Default_Error_Info
   );
   
   -- Simple Boolean Result
   package Simple_Boolean is new Async_Result.Simple (
      Value_Type => Boolean,
      Error_Type => Error_Info
   );

   -- ==========================================================================
   -- Convenience Functions
   -- ==========================================================================
   
   -- Create error info
   function Make_Error (
      Kind : Error_Kind; 
      Message : String := "";
      Code : Integer := 0
   ) return Error_Info;
   
   -- Convert error to string
   function Error_To_String (E : Error_Info) return String;
   
   -- Common error results
   function Invalid_Argument_Error (Message : String := "") 
                                  return Integer_Result.Result_Type;
   
   function Timeout_Error (Message : String := "") 
                         return Integer_Result.Result_Type;
   
   function IO_Error (Message : String := "") 
                    return String_Result.Result_Type;

   -- ==========================================================================
   -- Type Aliases for Convenience
   -- ==========================================================================
   
   -- Integer types
   subtype Int_Result is Integer_Result.Result_Type;
   subtype Int_Protected is Integer_Result.Protected_Result_Type;
   
   -- String types
   subtype Str_Result is String_Result.Result_Type;
   subtype Str_Protected is String_Result.Protected_Result_Type;
   
   -- Float types
   subtype Float_Result_Type is Float_Result.Result_Type;
   subtype Float_Protected is Float_Result.Protected_Result_Type;
   
   -- Boolean types
   subtype Bool_Result is Boolean_Result.Result_Type;
   subtype Bool_Protected is Boolean_Result.Protected_Result_Type;

   -- ==========================================================================
   -- Global Metrics Instance
   -- ==========================================================================
   
   -- Re-export global metrics for easy access
   package Metrics renames Async_Result.Metrics;
   
   -- Quick access to global metrics
   procedure Record_Success is null;
   procedure Record_Error is null;
   procedure Record_Async_Start is null;
   procedure Record_Async_Complete (Duration : Duration) is null;

end Async_Result.Prelude;