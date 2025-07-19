-- =============================================================================
-- Ada Async_Result Library - Simplified API
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Easy-to-use interface for common Result operations
-- Reduces boilerplate and provides sensible defaults
-- =============================================================================

with Ada.Strings.Unbounded;

generic
   -- Simplified generic parameters with common defaults
   type Value_Type is private;
   type Error_Type is private;
package Async_Result.Simple is
   pragma Elaborate_Body;

   use Ada.Strings.Unbounded;

   -- ==========================================================================
   -- Simplified Result Type
   -- ==========================================================================
   
   -- Re-export core types with simplified names
   subtype Result is Core.Result_Type;
   subtype State is Core.Result_State;
   
   Success : constant State := Core.Success;
   Error : constant State := Core.Error;

   -- ==========================================================================
   -- Creation Helpers
   -- ==========================================================================
   
   -- Create successful result
   function Ok (Value : Value_Type) return Result;
   
   -- Create error result
   function Err (Error : Error_Type) return Result;
   function Err (Error : Error_Type; Message : String) return Result;
   
   -- ==========================================================================
   -- Common Operations
   -- ==========================================================================
   
   -- Check result state
   function Is_Ok (R : Result) return Boolean renames Core.Is_Ok;
   function Is_Err (R : Result) return Boolean renames Core.Is_Err;
   
   -- Extract values with defaults
   function Value_Or (R : Result; Default : Value_Type) return Value_Type
      renames Core.Unwrap_Or;
   
   -- Get value (raises exception on error)
   function Value (R : Result) return Value_Type renames Core.Unwrap;
   
   -- Get error (raises exception on success)
   function Error (R : Result) return Error_Type renames Core.Unwrap_Err;
   
   -- Safe extraction
   function Get_Value (R : Result; Value : out Value_Type) return Boolean
      renames Core.Try_Get_Value;
      
   function Get_Error (R : Result; Error : out Error_Type) return Boolean
      renames Core.Try_Get_Error;

   -- ==========================================================================
   -- Chaining Operations
   -- ==========================================================================
   
   -- Simple map function
   generic
      type New_Value_Type is private;
      with function Transform (V : Value_Type) return New_Value_Type;
   function Map (R : Result) return Result;
   
   -- Simple error mapping
   generic
      with function Transform (E : Error_Type) return Error_Type;
   function Map_Error (R : Result) return Result;
   
   -- Simple and_then
   generic
      with function Transform (V : Value_Type) return Result;
   function And_Then (R : Result) return Result;
   
   -- Simple matching
   generic
      type Return_Type is private;
      with function If_Ok (V : Value_Type) return Return_Type;
      with function If_Err (E : Error_Type) return Return_Type;
   function Match (R : Result) return Return_Type;

   -- ==========================================================================
   -- Async Operations (Simplified)
   -- ==========================================================================
   
   -- Simple async transformation
   generic
      type New_Value_Type is private;
      with function Transform (V : Value_Type) return New_Value_Type;
   package Simple_Async is
      type Async_Handle is limited private;
      
      -- Start async operation
      function Start (R : Result) return Async_Handle;
      
      -- Wait for result
      function Wait_For (Handle : Async_Handle) return Result;
      
      -- Check if ready
      function Is_Ready (Handle : Async_Handle) return Boolean;
      
      -- Cancel operation
      procedure Cancel (Handle : in out Async_Handle);
      
   private
      type Async_Handle is limited record
         -- Implementation defined
         null;
      end record;
   end Simple_Async;

   -- ==========================================================================
   -- Common Patterns
   -- ==========================================================================
   
   -- Try pattern - execute and catch exceptions
   generic
      with function Try_Operation return Value_Type;
   function Try_Call return Result;
   
   -- Collect results from array
   type Result_Array is array (Positive range <>) of Result;
   
   function All_Ok (Results : Result_Array) return Boolean;
   function Any_Ok (Results : Result_Array) return Boolean;
   function First_Ok (Results : Result_Array) return Result;
   function First_Err (Results : Result_Array) return Result;
   
   -- Combine multiple results
   generic
      type Combined_Type is private;
      with function Combine (V1, V2 : Value_Type) return Combined_Type;
   function Combine_Results (R1, R2 : Result) return Result;

   -- ==========================================================================
   -- String Conversions
   -- ==========================================================================
   
   -- Convert to string for debugging
   generic
      with function Value_To_String (V : Value_Type) return String;
      with function Error_To_String (E : Error_Type) return String;
   function To_String (R : Result) return String;

   -- ==========================================================================
   -- Common Error Types
   -- ==========================================================================
   
   -- Pre-defined error types for convenience
   type Simple_Error is (
      None,
      Invalid_Input,
      Not_Found,
      Permission_Denied,
      Timeout,
      Network_Error,
      Parse_Error,
      Unknown_Error
   );
   
   -- Helper to create error results with simple errors
   function Err (Error : Simple_Error; Message : String := "") return Result;

   -- ==========================================================================
   -- Examples and Best Practices
   -- ==========================================================================
   
   -- Example 1: Simple value transformation
   -- declare
   --    R : Result := Ok (42);
   --    Doubled : Result := Map (R);  -- with Transform => Double
   -- begin
   --    if Is_Ok (Doubled) then
   --       Put_Line (Integer'Image (Value (Doubled)));
   --    end if;
   -- end;
   
   -- Example 2: Error handling pattern
   -- declare
   --    function Parse_Int (S : String) return Result is
   --    begin
   --       return Ok (Integer'Value (S));
   --    exception
   --       when others => return Err (Parse_Error, "Invalid integer");
   --    end Parse_Int;
   -- begin
   --    case Match (Parse_Int ("123")) is
   --       when Success => Put_Line ("Parsed successfully");
   --       when Error => Put_Line ("Parse failed");
   --    end case;
   -- end;
   
   -- Example 3: Chaining operations
   -- declare
   --    R : Result := Ok ("123")
   --       .And_Then (Parse_Int'Access)
   --       .Map (Double'Access)
   --       .Map_Error (Add_Context'Access);
   -- begin
   --    Put_Line (Value_Or (R, 0)'Image);
   -- end;

private
   -- Internal instantiation of Core
   package Core is new Async_Result.Core (
      Value_Type => Value_Type,
      Error_Type => Error_Type,
      Copy_Value => Copy_Value_Default,
      Copy_Error => Copy_Error_Default,
      Default_Value => Default_Value_Default,
      Default_Error => Default_Error_Default
   );
   
   -- Default functions for generic parameters
   function Copy_Value_Default (V : Value_Type) return Value_Type is (V);
   function Copy_Error_Default (E : Error_Type) return Error_Type is (E);
   
   function Default_Value_Default return Value_Type is
      V : Value_Type;
   begin
      return V;
   end Default_Value_Default;
   
   function Default_Error_Default return Error_Type is
      E : Error_Type;
   begin
      return E;
   end Default_Error_Default;

end Async_Result.Simple;