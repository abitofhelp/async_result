-- =============================================================================
-- Ada Async_Result Library - Core Result Types
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Core Result types following hybrid architecture and DIP principles
-- Provides the foundational Result<T, E> pattern similar to Rust
-- =============================================================================

with Ada.Finalization;
with Ada.Strings.Unbounded;

generic
   type Value_Type is private;
   type Error_Type is private;
   with function Copy_Value (Source : Value_Type) return Value_Type is <>;
   with function Copy_Error (Source : Error_Type) return Error_Type is <>;
   with function Default_Value return Value_Type is <>;
   with function Default_Error return Error_Type is <>;
package Async_Result.Core is
   pragma Elaborate_Body;

   use Ada.Strings.Unbounded;

   -- ==========================================================================
   -- Core Types
   -- ==========================================================================
   
   -- Core result state enumeration
   type Result_State is (Success, Error);

   -- Basic Result type - represents success or error
   type Result_Type is new Ada.Finalization.Controlled with private;

   -- ==========================================================================
   -- Construction Operations
   -- ==========================================================================

   -- Create a Result containing a successful value
   procedure Make_Ok (R : out Result_Type; Value : Value_Type)
      with Post => Is_Ok (R);
   
   -- Create a Result containing an error
   procedure Make_Err (R : out Result_Type; Err : Error_Type; Message : String := "")
      with Post => Is_Err (R);

   -- ==========================================================================
   -- Query Operations
   -- ==========================================================================

   -- Returns True if the Result contains a successful value
   function Is_Ok (R : Result_Type) return Boolean
      with Inline;
   
   -- Returns True if the Result contains an error  
   function Is_Err (R : Result_Type) return Boolean
      with Inline;
      
   -- Returns the internal state (Success or Error)
   function Get_State (R : Result_Type) return Result_State
      with Inline;

   -- ==========================================================================
   -- Extraction Operations
   -- ==========================================================================

   -- Extract the value from a successful Result (raises exception if error)
   function Unwrap (R : Result_Type) return Value_Type
      with Pre => Is_Ok (R);
   
   -- Extract the value, or return a default if the Result contains an error
   function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type;
   
   -- Extract the error from a failed Result (raises exception if success)
   function Unwrap_Err (R : Result_Type) return Error_Type
      with Pre => Is_Err (R);
   
   -- Get the error message if present
   function Get_Error_Message (R : Result_Type) return String
      with Pre => Is_Err (R);

   -- ==========================================================================
   -- Safe Extraction Operations
   -- ==========================================================================

   -- Try to get the value - returns True if successful, False if error
   function Try_Get_Value (R : Result_Type; Value : out Value_Type) return Boolean
      with Post => (if Try_Get_Value'Result then Is_Ok (R) else Is_Err (R));
   
   -- Try to get the error - returns True if successful, False if contains value
   function Try_Get_Error (R : Result_Type; Err : out Error_Type) return Boolean
      with Post => (if Try_Get_Error'Result then Is_Err (R) else Is_Ok (R));
   
   -- Try to get the error message - returns True if successful
   function Try_Get_Message (R : Result_Type; Message : out Unbounded_String) return Boolean
      with Post => (if Try_Get_Message'Result then Is_Err (R));

   -- ==========================================================================
   -- Pattern Matching
   -- ==========================================================================

   -- Visitor pattern for functional matching
   generic
      with procedure Handle_Ok (Value : Value_Type);
      with procedure Handle_Err (Err : Error_Type; Message : String);
   procedure Match (R : Result_Type);

   -- ==========================================================================
   -- Transformation Operations
   -- ==========================================================================
   
   -- Map operation - transform the success value if present
   generic
      type New_Value_Type is private;
      with procedure Transform (Input : Value_Type; Output : out New_Value_Type);
      with function Copy_New_Value (Source : New_Value_Type) return New_Value_Type is <>;
      with function Default_New_Value return New_Value_Type is <>;
   package Map_Operations is
      -- New result type for transformed values
      type New_Result_Type is new Ada.Finalization.Controlled with private;

      -- Transformation operation
      procedure Map (R : Result_Type; New_R : out New_Result_Type);
      
      -- Construction operations for new type
      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Err : Error_Type; Message : String := "");
      
      -- Query operations
      function Is_Ok (R : New_Result_Type) return Boolean;
      function Is_Err (R : New_Result_Type) return Boolean;
      function Get_State (R : New_Result_Type) return Result_State;
      
      -- Extraction operations
      function Unwrap (R : New_Result_Type) return New_Value_Type
         with Pre => Is_Ok (R);
      function Unwrap_Err (R : New_Result_Type) return Error_Type
         with Pre => Is_Err (R);
      function Get_Error_Message (R : New_Result_Type) return String
         with Pre => Is_Err (R);

   private
      type New_Result_Type is new Ada.Finalization.Controlled with record
         State : Result_State := Error;
         Value : New_Value_Type := Default_New_Value;
         Error : Error_Type := Default_Error;
         Error_Message : Unbounded_String := Null_Unbounded_String;
         Is_Initialized : Boolean := False;
      end record;
      
      -- Resource management
      overriding procedure Initialize (Object : in out New_Result_Type);
      overriding procedure Adjust (Object : in out New_Result_Type);
      overriding procedure Finalize (Object : in out New_Result_Type);
   end Map_Operations;

   -- And_Then operation - chain operations that might fail
   generic
      with procedure Transform (Input : Value_Type; Output : out Result_Type);
   package And_Then_Operations is
      procedure And_Then (R : Result_Type; New_R : out Result_Type);
   end And_Then_Operations;

   -- ==========================================================================
   -- Concurrent Result Type
   -- ==========================================================================
   
   -- Thread-safe result wrapper using protected types
   protected type Protected_Result_Type is
      -- Factory operations
      procedure Make_Ok (Value : Value_Type);
      procedure Make_Err (Err : Error_Type; Message : String := "");
      
      -- Query operations
      function Is_Ok return Boolean;
      function Is_Err return Boolean;
      function Get_State return Result_State;
      
      -- Non-blocking operations
      function Try_Get_Value (Value : out Value_Type) return Boolean;
      function Try_Get_Error (Err : out Error_Type; 
                             Message : out Unbounded_String) return Boolean;
      
      -- Blocking operation
      entry Wait_For_Result (R : out Result_Type);
      
   private
      Result : Result_Type;
      Is_Set : Boolean := False;
   end Protected_Result_Type;

   -- ==========================================================================
   -- Exceptions
   -- ==========================================================================
   
   -- Result validation exception
   Result_Not_Valid : exception;
   
   -- Unwrap called on error Result
   Unwrap_Error : exception;

   -- ==========================================================================
   -- Utility Functions
   -- ==========================================================================
   
   -- Type conversion helpers
   function To_Result (Value : Value_Type) return Result_Type;
   function To_Error_Result (Err : Error_Type; Message : String := "") return Result_Type;
   
   -- Get a human-readable representation of the Result
   function To_String (R : Result_Type) return String;

private
   -- Private implementation following information hiding principle
   type Result_Type is new Ada.Finalization.Controlled with record
      State : Result_State := Error;
      Value : Value_Type := Default_Value;
      Error : Error_Type := Default_Error;
      Error_Message : Unbounded_String := Null_Unbounded_String;
      Is_Initialized : Boolean := False;
   end record;

   -- Resource management operations
   overriding procedure Initialize (Object : in out Result_Type);
   overriding procedure Adjust (Object : in out Result_Type);
   overriding procedure Finalize (Object : in out Result_Type);

   -- Internal validation
   procedure Ensure_Valid_State (R : Result_Type)
      with Inline;

end Async_Result.Core;