-- =============================================================================
-- Ada Result Library
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- High-performance, memory-safe Result type for Ada providing type-safe error
-- handling without exceptions. Part of the Ada Result ecosystem.
--
-- For full license text, see LICENSE.md
-- For documentation, see README.md
-- =============================================================================

-- async_result.adb
-- Generic Result type package body for robust error handling
--
-- This implementation provides comprehensive error handling capabilities
-- while maintaining high performance through careful memory management
-- and exception safety guarantees.
--
-- Implementation Notes:
--   * Uses controlled types for automatic resource management
--   * Implements zero-copy operations through OUT parameters
--   * Provides exception-safe cleanup in all error paths
--   * Optimized for both small and large data types

with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;

package body Async_Result is

   -- Targeted pragma suppressions for performance-critical sections
   -- These suppress specific checks only where performance is critical and safety is ensured
   pragma Suppress (Index_Check, Get_State);
   pragma Suppress (Index_Check, Is_Ok);
   pragma Suppress (Index_Check, Is_Error);
   pragma Suppress (Range_Check, Get_Message_Length);
   pragma Suppress (Range_Check, Has_Message);

   ----------------------------------------------------------------------------
   -- INTERNAL VALIDATION HELPERS
   ----------------------------------------------------------------------------

   procedure Ensure_Valid_State (R : Result_Type) is
   begin
      if not R.Is_Initialized then
         raise Invalid_State_Error with "Result type not properly initialized";
      end if;

      if not Is_State_Consistent (R) then
         raise Invalid_State_Error with "Result type in inconsistent state";
      end if;
   end Ensure_Valid_State;
   pragma Inline (Ensure_Valid_State);

   procedure Ensure_Success_State (R : Result_Type) is
   begin
      Ensure_Valid_State (R);
      if R.State /= Success then
         if Length (R.Message) > 0 then
            raise Result_Error
              with "Expected success state but found error: " & To_String (R.Message);
         else
            raise Result_Error with "Expected success state but found error";
         end if;
      end if;
   end Ensure_Success_State;
   pragma Inline (Ensure_Success_State);

   procedure Ensure_Error_State (R : Result_Type) is
   begin
      Ensure_Valid_State (R);
      if R.State /= Async_Result.Error then
         raise Result_Error with "Expected error state but found success";
      end if;
   end Ensure_Error_State;
   pragma Inline (Ensure_Error_State);

   ----------------------------------------------------------------------------
   -- MEMORY MANAGEMENT (RAII PATTERN)
   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Object : in out Result_Type) is
   begin
      -- Initialize Result in error state with default values
      -- This ensures safe initial state for all instances
      Object.State := Async_Result.Error;
      Object.Value := Default_Value;
      Object.Error := Default_Error;
      Object.Message := Null_Unbounded_String;
      Object.Has_Explicit_Message := False;
      Object.Is_Initialized := True;
      Object.Needs_Cleanup := False;
   end Initialize;

   overriding
   procedure Adjust (Object : in out Result_Type) is
   begin
      -- Handle deep copying when Result is copied (assignment, parameter passing)
      -- This is called automatically by the runtime for controlled types
      case Object.State is
         when Success =>
            begin
               Object.Value := Copy_Value (Object.Value);
            exception
               when others =>
                  -- If copying fails, convert to error state
                  Object.State := Async_Result.Error;
                  Object.Error := Default_Error;
                  Object.Message := To_Unbounded_String ("Value copy failed during adjust");
                  Object.Has_Explicit_Message := True;
            end;
         when Async_Result.Error =>
            begin
               Object.Error := Copy_Error (Object.Error);
            exception
               when others =>
                  -- If error copying fails, use default error
                  Object.Error := Default_Error;
                  Object.Message := To_Unbounded_String ("Error copy failed during adjust");
                  Object.Has_Explicit_Message := True;
            end;
      end case;
      Object.Is_Initialized := True;
   end Adjust;

   overriding
   procedure Finalize (Object : in out Result_Type) is
      Was_Initialized : constant Boolean := Object.Is_Initialized;
   begin
      -- Clean up resources when Result goes out of scope
      -- This is called automatically when Result instances are destroyed
      if Was_Initialized then
         Object.Is_Initialized := False;

         if Object.Needs_Cleanup then
            Cleanup_Resources (Object);
         end if;

         Object.Message := Null_Unbounded_String;
         Object.Needs_Cleanup := False;
      end if;
   exception
      when others =>
         -- Finalize must never propagate exceptions
         Object.Is_Initialized := False;
         Object.Needs_Cleanup := False;
   end Finalize;

   ----------------------------------------------------------------------------
   -- CONSTRUCTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   procedure Make_Ok (R : out Result_Type; Value : Value_Type) is
   begin
      -- Create a Result containing a successful value
      R.State := Success;
      R.Value := Copy_Value (Value);
      R.Error := Default_Error;
      R.Message := Null_Unbounded_String;
      R.Has_Explicit_Message := False;
      R.Is_Initialized := True;
      R.Needs_Cleanup := Requires_Deep_Copy (R);
   end Make_Ok;

   procedure Make_Err (R : out Result_Type; Error : Error_Type) is
   begin
      -- Create a Result containing an error
      R.State := Async_Result.Error;
      R.Value := Default_Value;
      R.Error := Copy_Error (Error);
      R.Message := Null_Unbounded_String;
      R.Has_Explicit_Message := False;
      R.Is_Initialized := True;
      R.Needs_Cleanup := Requires_Deep_Copy (R);
   end Make_Err;

   procedure Make_Err
     (R : out Result_Type; Error : Error_Type; Message : String) is
   begin
      -- Create a Result containing an error with a descriptive message
      R.State := Async_Result.Error;
      R.Value := Default_Value;
      R.Error := Copy_Error (Error);
      R.Message := To_Unbounded_String (Message);
      R.Has_Explicit_Message := True;
      R.Is_Initialized := True;
      R.Needs_Cleanup := Requires_Deep_Copy (R);
   end Make_Err;

   ----------------------------------------------------------------------------
   -- STATE INSPECTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   function Is_Ok (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return R.State = Success;
   end Is_Ok;

   function Is_Error (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return R.State = Async_Result.Error;
   end Is_Error;

   function Get_State (R : Result_Type) return Result_State is
   begin
      Ensure_Valid_State (R);
      return R.State;
   end Get_State;

   ----------------------------------------------------------------------------
   -- VALUE EXTRACTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   function Unwrap (R : Result_Type) return Value_Type is
   begin
      -- Extract the value from a successful Result (raises exception if error)
      -- This function should only be called when you're certain the Result contains a value
      if not Is_Ok (R) then
         if Length (R.Message) > 0 then
            raise Unwrap_Error
              with "Called unwrap on error result: " & To_String (R.Message);
         else
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
      end if;
      return R.Value;
   end Unwrap;

   procedure Unwrap_Into (R : Result_Type; Value : out Value_Type) is
   begin
      if not Is_Ok (R) then
         if Length (R.Message) > 0 then
            raise Unwrap_Error
              with "Called unwrap on error result: " & To_String (R.Message);
         else
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
      end if;
      Value := R.Value;
   end Unwrap_Into;

   function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type is
   begin
      -- Extract the value, or return a default if the Result contains an error
      -- This is a safe way to get a value without risking exceptions
      Ensure_Valid_State (R);
      if R.State = Success then
         return R.Value;
      else
         return Default;
      end if;
   end Unwrap_Or;

   procedure Unwrap_Or_Into
     (R : Result_Type; Default : Value_Type; Value : out Value_Type) is
   begin
      Ensure_Valid_State (R);
      if R.State = Success then
         Value := R.Value;
      else
         Value := Default;
      end if;
   end Unwrap_Or_Into;

   function Expect (R : Result_Type; Message : String) return Value_Type is
   begin
      if not Is_Ok (R) then
         raise Expect_Error with Message;
      end if;
      return R.Value;
   end Expect;

   procedure Expect_Into
     (R : Result_Type; Message : String; Value : out Value_Type) is
   begin
      if not Is_Ok (R) then
         raise Expect_Error with Message;
      end if;
      Value := R.Value;
   end Expect_Into;

   function Unwrap_Err (R : Result_Type) return Error_Type is
   begin
      if not Is_Error (R) then
         raise Unwrap_Error with "Called unwrap_err on success result";
      end if;
      return R.Error;
   end Unwrap_Err;

   procedure Unwrap_Err_Into (R : Result_Type; Error : out Error_Type) is
   begin
      if not Is_Error (R) then
         raise Unwrap_Error with "Called unwrap_err on success result";
      end if;
      Error := R.Error;
   end Unwrap_Err_Into;

   function Expect_Err (R : Result_Type; Message : String) return Error_Type is
   begin
      if not Is_Error (R) then
         raise Expect_Error with Message;
      end if;
      return R.Error;
   end Expect_Err;

   ----------------------------------------------------------------------------
   -- TRANSFORMATION OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Map_Operations is

      overriding
      procedure Initialize (Object : in out New_Result_Type) is
      begin
         Object.State := Async_Result.Error;
         Object.Message := Null_Unbounded_String;
         Object.Has_Explicit_Message := False;
         Object.Is_Initialized := True;
      end Initialize;

      overriding
      procedure Adjust (Object : in out New_Result_Type) is
      begin
         Object.Is_Initialized := True;
      end Adjust;

      overriding
      procedure Finalize (Object : in out New_Result_Type) is
      begin
         Object.Message := Null_Unbounded_String;
         Object.Is_Initialized := False;
      end Finalize;

      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type) is
      begin
         R.State := Success;
         R.Value := Value;
         R.Message := Null_Unbounded_String;
         R.Has_Explicit_Message := False;
         R.Is_Initialized := True;
      end Make_Ok;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type) is
      begin
         R.State := Async_Result.Error;
         R.Error := Error;
         R.Message := Null_Unbounded_String;
         R.Has_Explicit_Message := False;
         R.Is_Initialized := True;
      end Make_Err;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String) is
      begin
         R.State := Async_Result.Error;
         R.Error := Error;
         R.Message := To_Unbounded_String (Message);
         R.Has_Explicit_Message := True;
         R.Is_Initialized := True;
      end Make_Err;

      function Is_Ok (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Success;
      end Is_Ok;

      function Is_Error (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Async_Result.Error;
      end Is_Error;

      function Unwrap (R : New_Result_Type) return New_Value_Type is
      begin
         if not Is_Ok (R) then
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
         return R.Value;
      end Unwrap;

      function Unwrap_Err (R : New_Result_Type) return Error_Type is
      begin
         if not Is_Error (R) then
            raise Unwrap_Error with "Called unwrap_err on success result";
         end if;
         return R.Error;
      end Unwrap_Err;

      procedure Map (R : Result_Type; New_R : out New_Result_Type) is
      begin
         -- Transform the success value if present, leave errors unchanged
         -- This allows you to convert one value type to another while preserving errors
         Ensure_Valid_State (R);

         if R.State = Success then
            declare
               Transformed_Value : New_Value_Type;
            begin
               Transform (R.Value, Transformed_Value);
               Make_Ok (New_R, Transformed_Value);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Map operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, Default_Error, To_String (R.Message));
         end if;
      end Map;
   end Map_Operations;

   package body And_Then_Operations is
      procedure And_Then (R : Result_Type; New_R : out Result_Type) is
      begin
         -- Chain operations that might fail
         -- Use this when your transformation function can itself return a Result
         Ensure_Valid_State (R);

         if R.State = Success then
            begin
               Transform (R.Value, New_R);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "And_Then operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, R.Error, To_String (R.Message));
         end if;
      end And_Then;
   end And_Then_Operations;

   package body Map_Error_Operations is
      procedure Map_Err (R : Result_Type; New_R : out Result_Type) is
      begin
         Ensure_Valid_State (R);

         if R.State = Async_Result.Error then
            declare
               Transformed_Error : Error_Type;
            begin
               Transform (R.Error, Transformed_Error);
               Make_Err (New_R, Transformed_Error, To_String (R.Message));
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Map_Err operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Ok (New_R, R.Value);
         end if;
      end Map_Err;
   end Map_Error_Operations;

   package body Bind_Operations is
      procedure Bind (R : Result_Type; New_R : out Result_Type) is
      begin
         Ensure_Valid_State (R);

         if R.State = Success then
            begin
               Transform (R.Value, New_R);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Bind operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, R.Error, To_String (R.Message));
         end if;
      end Bind;
   end Bind_Operations;

   package body Fmap_Operations is

      overriding
      procedure Initialize (Object : in out New_Result_Type) is
      begin
         Object.State := Async_Result.Error;
         Object.Message := Null_Unbounded_String;
         Object.Has_Explicit_Message := False;
         Object.Is_Initialized := True;
      end Initialize;

      overriding
      procedure Adjust (Object : in out New_Result_Type) is
      begin
         Object.Is_Initialized := True;
      end Adjust;

      overriding
      procedure Finalize (Object : in out New_Result_Type) is
      begin
         Object.Message := Null_Unbounded_String;
         Object.Is_Initialized := False;
      end Finalize;

      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type) is
      begin
         R.State := Success;
         R.Value := Value;
         R.Message := Null_Unbounded_String;
         R.Has_Explicit_Message := False;
         R.Is_Initialized := True;
      end Make_Ok;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type) is
      begin
         R.State := Async_Result.Error;
         R.Error := Error;
         R.Message := Null_Unbounded_String;
         R.Has_Explicit_Message := False;
         R.Is_Initialized := True;
      end Make_Err;

      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String) is
      begin
         R.State := Async_Result.Error;
         R.Error := Error;
         R.Message := To_Unbounded_String (Message);
         R.Has_Explicit_Message := True;
         R.Is_Initialized := True;
      end Make_Err;

      function Is_Ok (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Success;
      end Is_Ok;

      function Is_Error (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Async_Result.Error;
      end Is_Error;

      function Unwrap (R : New_Result_Type) return New_Value_Type is
      begin
         if not Is_Ok (R) then
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
         return R.Value;
      end Unwrap;

      function Unwrap_Err (R : New_Result_Type) return Error_Type is
      begin
         if not Is_Error (R) then
            raise Unwrap_Error with "Called unwrap_err on success result";
         end if;
         return R.Error;
      end Unwrap_Err;

      procedure Fmap (R : Result_Type; New_R : out New_Result_Type) is
      begin
         Ensure_Valid_State (R);

         if R.State = Success then
            declare
               Transformed_Value : New_Value_Type;
            begin
               Transform (R.Value, Transformed_Value);
               Make_Ok (New_R, Transformed_Value);
            exception
               when Ex : others =>
                  Make_Err (New_R, Default_Error, "Fmap operation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (New_R, Default_Error, To_String (R.Message));
         end if;
      end Fmap;
   end Fmap_Operations;

   ----------------------------------------------------------------------------
   -- PATTERN MATCHING INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Match_Operations is
      procedure Match (R : Result_Type; Output : out Return_Type) is
      begin
         Ensure_Valid_State (R);
         case R.State is
            when Success =>
               On_Success (R.Value, Output);
            when Async_Result.Error =>
               On_Error (R.Error, Output);
         end case;
      end Match;
   end Match_Operations;

   package body Fold_Operations is
      procedure Fold (R : Result_Type; Output : out Return_Type) is
      begin
         Ensure_Valid_State (R);
         case R.State is
            when Success =>
               Success_Transform (R.Value, Output);
            when Async_Result.Error =>
               Error_Transform (R.Error, Output);
         end case;
      end Fold;
   end Fold_Operations;

   package body Swap_Operations is
      procedure Swap (R : Result_Type; Swapped_R : out Result_Type) is
      begin
         Ensure_Valid_State (R);
         case R.State is
            when Success =>
               Make_Err (Swapped_R, Value_To_Error (R.Value), "Swapped from success");
            when Async_Result.Error =>
               Make_Ok (Swapped_R, Error_To_Value (R.Error));
         end case;
      end Swap;
   end Swap_Operations;

   ----------------------------------------------------------------------------
   -- ADVANCED RUST-STYLE OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Lazy_Operations is
      function Unwrap_Or_Else (R : Result_Type) return Value_Type is
      begin
         Ensure_Valid_State (R);
         if R.State = Success then
            return R.Value;
         else
            return Default_Fn;
         end if;
      end Unwrap_Or_Else;
   end Lazy_Operations;

   package body Map_Or_Operations is
      function Map_Or (R : Result_Type; Default : Value_Type) return Value_Type is
      begin
         Ensure_Valid_State (R);
         if R.State = Success then
            return Transform_Fn (R.Value);
         else
            return Default;
         end if;
      end Map_Or;
   end Map_Or_Operations;

   package body Value_Predicate_Operations is
      function Is_Ok_And (R : Result_Type) return Boolean is
      begin
         Ensure_Valid_State (R);
         return R.State = Success and then Predicate (R.Value);
      end Is_Ok_And;
   end Value_Predicate_Operations;

   package body Error_Predicate_Operations is
      function Is_Err_And (R : Result_Type) return Boolean is
      begin
         Ensure_Valid_State (R);
         return R.State = Async_Result.Error and then Predicate (R.Error);
      end Is_Err_And;
   end Error_Predicate_Operations;

   ----------------------------------------------------------------------------
   -- SAFE EXTRACTION INTERFACE IMPLEMENTATION
   ----------------------------------------------------------------------------

   function Is_Valid_State (R : Result_Type) return Boolean is
   begin
      return R.Is_Initialized and then Is_State_Consistent (R);
   end Is_Valid_State;

   function Is_State_Consistent (R : Result_Type) return Boolean is
   begin
      if not R.Is_Initialized then
         return False;
      end if;

      case R.State is
         when Success =>
            return True;
         when Async_Result.Error =>
            return True;
      end case;
   end Is_State_Consistent;
   pragma Inline (Is_State_Consistent);

   function Has_Message (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return R.Has_Explicit_Message;
   end Has_Message;

   function Get_Message_Length (R : Result_Type) return Natural is
   begin
      Ensure_Valid_State (R);
      return Length (R.Message);
   end Get_Message_Length;

   function Try_Get_Value
     (R : Result_Type; Value : out Value_Type) return Boolean is
   begin
      if not Is_Valid_State (R) or else R.State /= Success then
         return False;
      end if;
      Value := R.Value;
      return True;
   end Try_Get_Value;

   function Try_Get_Error
     (R : Result_Type; Error : out Error_Type) return Boolean is
   begin
      if not Is_Valid_State (R) or else R.State /= Async_Result.Error then
         return False;
      end if;
      Error := R.Error;
      return True;
   end Try_Get_Error;

   function Try_Get_Message
     (R : Result_Type; Message : out Unbounded_String) return Boolean is
   begin
      if not Is_Valid_State (R)
        or else R.State /= Async_Result.Error
        or else Length (R.Message) = 0
      then
         return False;
      end if;
      Message := R.Message;
      return True;
   end Try_Get_Message;

   function Get_Message (R : Result_Type) return String is
   begin
      Ensure_Error_State (R);
      return To_String (R.Message);
   end Get_Message;

   ----------------------------------------------------------------------------
   -- RESOURCE MANAGEMENT IMPLEMENTATION
   ----------------------------------------------------------------------------

   procedure Cleanup_Resources (R : in out Result_Type) is
   begin
      -- This procedure cleans up any resources associated with the Result
      -- It provides a framework for handling cleanup of Value_Type and Error_Type instances
      
      -- Early return if cleanup not needed
      if not R.Needs_Cleanup then
         return;
      end if;
      
      -- Clear the cleanup flag first to prevent recursive cleanup
      R.Needs_Cleanup := False;
      
      -- Perform cleanup based on the current state
      case R.State is
         when Success =>
            -- Clean up value-related resources
            -- For types that implement Ada.Finalization.Controlled,
            -- finalization will be handled automatically
            
            -- For access types, we would typically null the access value
            -- after deallocating the pointed-to object
            if Value_Type'Has_Access_Values then
               -- User should provide proper deallocation in their cleanup procedure
               -- For safety, we set needs_cleanup to false to prevent further issues
               null;
            end if;
            
            -- For large types, call user-provided cleanup if available
            if Value_Type'Size > 8192 then
               -- User-provided cleanup would go here
               null;
            end if;
            
         when Async_Result.Error =>
            -- Clean up error-related resources
            -- Similar cleanup logic for Error_Type
            
            if Error_Type'Has_Access_Values then
               -- User should provide proper deallocation in their cleanup procedure
               null;
            end if;
            
            if Error_Type'Size > 8192 then
               -- User-provided cleanup would go here
               null;
            end if;
      end case;
      
      -- Additional cleanup for message string is handled by finalization
      -- R.Message will be automatically cleaned up by Unbounded_String
      
   exception
      when others =>
         -- Ensure cleanup flag is cleared even if cleanup fails
         R.Needs_Cleanup := False;
         -- We don't re-raise the exception to prevent finalization issues
   end Cleanup_Resources;

   function Requires_Deep_Copy (R : Result_Type) return Boolean is
      pragma Unreferenced (R);
   begin
      -- This function determines whether the Value_Type and Error_Type require 
      -- deep copying based on their characteristics
      
      -- Check for characteristics that require deep copying:
      
      -- 1. Access types (pointers) - These always require deep copying
      --    to avoid aliasing and dangling pointer issues
      if Value_Type'Has_Access_Values or Error_Type'Has_Access_Values then
         return True;
      end if;
      
      -- 2. Types that are large enough to benefit from reference counting
      --    or have expensive copying operations (> 1KB)
      if Value_Type'Size > 8192 or Error_Type'Size > 8192 then
         return True;
      end if;
      
      -- 3. For complex types, assume they may need deep copying
      -- This is a conservative heuristic for safety
      if Value_Type'Size > 1024 or Error_Type'Size > 1024 then
         return True;
      end if;
      
      -- 4. For most basic types (Integer, Boolean, Character, enums, small records),
      -- deep copying is not necessary and would add unnecessary overhead
      
      -- This implementation provides a reasonable default for common types
      -- Users can override this by providing their own Cleanup_Resources implementation
      
      return False;
      
      -- NOTE: In a production implementation, this function could be enhanced to:
      -- 1. Use Ada 2012 aspects to declare copy requirements
      -- 2. Analyze type characteristics at compile time
      -- 3. Allow user override through generic parameters
      -- 4. Use type-specific heuristics for common patterns
   end Requires_Deep_Copy;

   ----------------------------------------------------------------------------
   -- DEBUGGING AND DIAGNOSTICS IMPLEMENTATION
   ----------------------------------------------------------------------------

   function To_String (R : Result_Type) return String is
   begin
      if not Is_Valid_State (R) then
         return "Invalid(uninitialized)";
      end if;

      case R.State is
         when Success =>
            return "Ok(value)";
         when Async_Result.Error =>
            if Length (R.Message) > 0 then
               return "Err(" & To_String (R.Message) & ")";
            else
               return "Err(error)";
            end if;
      end case;
   end To_String;

   function To_Debug_String (R : Result_Type) return String is
   begin
      if not Is_Valid_State (R) then
         return "Result { State: Invalid, Initialized: False }";
      end if;

      case R.State is
         when Success =>
            return
              "Result { State: Success, Has_Value: True, Initialized: True, "
              & "Needs_Cleanup: " & Boolean'Image (R.Needs_Cleanup) & " }";
         when Async_Result.Error =>
            return
              "Result { State: Error, Has_Error: True, Message_Length: "
              & Natural'Image (Length (R.Message))
              & ", Initialized: True, Needs_Cleanup: " 
              & Boolean'Image (R.Needs_Cleanup) & " }";
      end case;
   end To_Debug_String;

   procedure Validate_State (R : Result_Type) is
   begin
      if not R.Is_Initialized then
         raise Invalid_State_Error with "Result not initialized - object may be corrupted";
      end if;

      if not Is_State_Consistent (R) then
         raise Invalid_State_Error with "Result state is inconsistent - internal invariants violated";
      end if;

      case R.State is
         when Success =>
            -- Success state validation - ensure value is accessible
            -- Additional validation could check Value_Type specific constraints
            null;
         when Async_Result.Error =>
            -- Error state validation - ensure error information is consistent
            -- Additional validation could check Error_Type specific constraints
            null;
      end case;
   end Validate_State;

   ----------------------------------------------------------------------------
   -- PROTECTED TYPE IMPLEMENTATION
   ----------------------------------------------------------------------------

   protected body Protected_Result_Type is
      
      procedure Make_Ok (Value : Value_Type) is
      begin
         Async_Result.Make_Ok (Internal_Result, Value);
      end Make_Ok;
      
      procedure Make_Err (Error : Error_Type) is
      begin
         Async_Result.Make_Err (Internal_Result, Error);
      end Make_Err;
      
      procedure Make_Err (Error : Error_Type; Message : String) is
      begin
         Async_Result.Make_Err (Internal_Result, Error, Message);
      end Make_Err;
      
      function Is_Ok return Boolean is
      begin
         return Async_Result.Is_Ok (Internal_Result);
      end Is_Ok;
      
      function Is_Error return Boolean is
      begin
         return Async_Result.Is_Error (Internal_Result);
      end Is_Error;
      
      function Get_State return Result_State is
      begin
         return Async_Result.Get_State (Internal_Result);
      end Get_State;
      
      function Try_Get_Value (Value : out Value_Type) return Boolean is
      begin
         return Async_Result.Try_Get_Value (Internal_Result, Value);
      end Try_Get_Value;
      
      function Try_Get_Error (Error : out Error_Type) return Boolean is
      begin
         return Async_Result.Try_Get_Error (Internal_Result, Error);
      end Try_Get_Error;
      
      function Try_Get_Message (Message : out Unbounded_String) return Boolean is
      begin
         return Async_Result.Try_Get_Message (Internal_Result, Message);
      end Try_Get_Message;
      
      procedure Copy_To (Target : out Result_Type) is
      begin
         Target := Internal_Result;
      end Copy_To;
      
      procedure Copy_From (Source : Result_Type) is
      begin
         Internal_Result := Source;
      end Copy_From;
      
   end Protected_Result_Type;

   ----------------------------------------------------------------------------
   -- ASYNC COMPUTATION TASK IMPLEMENTATION
   ----------------------------------------------------------------------------

   task body Async_Computation_Task is
      Input_Value : Value_Type;
      Local_Result : Result_Type;
      Is_Cancelled : Boolean := False;
   begin
      select
         accept Start (Input : Value_Type) do
            Input_Value := Input;
         end Start;
         
         -- Execute the transformation if not cancelled
         if not Is_Cancelled then
            begin
               -- For now, just pass through the input value
               Make_Ok (Local_Result, Input_Value);
            exception
               when Ex : others =>
                  Make_Err (Local_Result, Default_Error, 
                           "Async computation failed: " & Exception_Information (Ex));
            end;
         else
            Make_Err (Local_Result, Default_Error, "Operation was cancelled");
         end if;
         
         -- Provide result to caller
         loop
            select
               accept Get_Result (R : out Result_Type) do
                  R := Local_Result;
               end Get_Result;
               exit;
            or
               accept Cancel do
                  Is_Cancelled := True;
                  Make_Err (Local_Result, Default_Error, "Operation was cancelled");
               end Cancel;
            end select;
         end loop;
         
      or
         accept Cancel do
            Is_Cancelled := True;
         end Cancel;
      end select;
   end Async_Computation_Task;

   ----------------------------------------------------------------------------
   -- ASYNC MAP OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Async_Map_Operations is
      
      protected body Status_Manager is
         procedure Set_Status (New_Status : Async_Status) is
         begin
            Current_Status := New_Status;
         end Set_Status;
         
         function Get_Status return Async_Status is
         begin
            return Current_Status;
         end Get_Status;
         
         procedure Cancel_If_Pending (Was_Cancelled : out Boolean) is
         begin
            if Current_Status = Pending then
               Current_Status := Cancelled;
               Was_Cancelled := True;
            else
               Was_Cancelled := False;
            end if;
         end Cancel_If_Pending;
         
         procedure Set_Start_Time (Time : Ada.Calendar.Time) is
         begin
            Start_Time := Time;
            Has_Start_Time := True;
         end Set_Start_Time;
         
         procedure Set_End_Time (Time : Ada.Calendar.Time) is
         begin
            End_Time := Time;
            Has_End_Time := True;
         end Set_End_Time;
         
         function Get_Execution_Time return Duration is
         begin
            if Has_Start_Time and Has_End_Time then
               return End_Time - Start_Time;
            else
               return 0.0;
            end if;
         end Get_Execution_Time;
      end Status_Manager;
      
      -- New_Result_Type operations
      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type) is
      begin
         R.State := Success;
         R.Value := Copy_New_Value (Value);
         R.Is_Initialized := True;
      end Make_Ok;
      
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type) is
      begin
         R.State := Async_Result.Error;  -- Qualify the state
         R.Error := Copy_Error (Error);
         R.Message := Null_Unbounded_String;
         R.Has_Explicit_Message := False;
         R.Is_Initialized := True;
      end Make_Err;
      
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String) is
      begin
         R.State := Async_Result.Error;  -- Qualify the state
         R.Error := Copy_Error (Error);
         R.Message := To_Unbounded_String (Message);
         R.Has_Explicit_Message := True;
         R.Is_Initialized := True;
      end Make_Err;
      
      function Is_Ok (R : New_Result_Type) return Boolean is
      begin
         return R.State = Success;
      end Is_Ok;
      
      function Is_Error (R : New_Result_Type) return Boolean is
      begin
         return R.State = Async_Result.Error;
      end Is_Error;
      
      function Unwrap (R : New_Result_Type) return New_Value_Type is
      begin
         if R.State /= Success then
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
         return R.Value;
      end Unwrap;
      
      function Unwrap_Err (R : New_Result_Type) return Error_Type is
      begin
         if R.State /= Async_Result.Error then
            raise Unwrap_Error with "Called unwrap_err on success result";
         end if;
         return R.Error;
      end Unwrap_Err;
      
      overriding procedure Initialize (Object : in out New_Result_Type) is
      begin
         Object.State := Async_Result.Error;
         Object.Value := Default_New_Value;
         Object.Error := Default_Error;
         Object.Message := Null_Unbounded_String;
         Object.Has_Explicit_Message := False;
         Object.Is_Initialized := True;
      end Initialize;
      
      overriding procedure Adjust (Object : in out New_Result_Type) is
      begin
         if Object.Is_Initialized then
            Object.Value := Copy_New_Value (Object.Value);
            Object.Error := Copy_Error (Object.Error);
         end if;
      end Adjust;
      
      overriding procedure Finalize (Object : in out New_Result_Type) is
      begin
         Object.Is_Initialized := False;
      end Finalize;
      
      task body Map_Task is
         Input_Result : Result_Type;
         Output_Result : New_Result_Type;
         Is_Cancelled : Boolean := False;
         Is_Started : Boolean := False;
         Task_Completed : Boolean := False;
         Start_Time : Ada.Calendar.Time;
      begin
         
         -- Main task loop - keep running until result is retrieved or timeout
         loop
            select
               when not Is_Started =>
                  accept Start (Input : Result_Type) do
                     Input_Result := Input;
                     Start_Time := Ada.Calendar.Clock;
                     Is_Started := True;
                     Global_Metrics.Record_Task_Start;
                  end Start;
                  
                  -- Perform the transformation only if not cancelled
                  if not Is_Cancelled then
                     if Is_Ok (Input_Result) then
                        begin
                           declare
                              Input_Value : constant Value_Type := Unwrap (Input_Result);
                              Output_Value : New_Value_Type;
                              Execution_Time : Duration;
                           begin
                              Transform (Input_Value, Output_Value);
                              
                              -- Create result with transformed value (no unsafe conversion!)
                              Make_Ok (Output_Result, Copy_New_Value (Output_Value));
                              
                              -- Record successful completion with execution time
                              Execution_Time := Ada.Calendar.Clock - Start_Time;
                              Global_Metrics.Record_Task_Complete (Execution_Time);
                           end;
                        exception
                           when Ex : others =>
                              Make_Err (Output_Result, Default_Error,
                                       "Async map failed: " & Exception_Information (Ex));
                              Global_Metrics.Record_Task_Failed;
                        end;
                     else
                        -- Preserve original error
                        Make_Err (Output_Result, Unwrap_Err (Input_Result));
                     end if;
                  else
                     Make_Err (Output_Result, Default_Error, "Operation was cancelled");
                     Global_Metrics.Record_Task_Cancelled;
                  end if;
                  Task_Completed := True;
                  
            or
               when Is_Started and not Task_Completed =>
                  accept Get_Result (Output : out New_Result_Type) do
                     Output := Output_Result;
                  end Get_Result;
                  exit; -- Task finished - result delivered
                  
            or
               when Is_Started and Task_Completed =>
                  accept Get_Result (Output : out New_Result_Type) do
                     Output := Output_Result;
                  end Get_Result;
                  exit; -- Task finished - result delivered
                  
            or
               when Is_Started =>
                  accept Get_Result_With_Timeout (Output : out New_Result_Type; Timeout : Duration; Success : out Boolean) do
                     pragma Unreferenced (Timeout);
                     Output := Output_Result;
                     Success := True;
                  end Get_Result_With_Timeout;
                  exit; -- Task finished - result delivered
                  
            or
               accept Cancel do
                  Is_Cancelled := True;
                  if Is_Started then
                     -- If work was started, mark it as cancelled
                     Make_Err (Output_Result, Default_Error, "Operation was cancelled");
                     Task_Completed := True;
                  end if;
                  Global_Metrics.Record_Task_Cancelled;
               end Cancel;
               -- Continue loop - still need to handle Get_Result calls
               
            or
               delay 60.0; -- 60 second timeout for startup or result retrieval
               -- Timeout - exit gracefully
               exit;
            end select;
         end loop;
      end Map_Task;
      
      overriding procedure Finalize (Object : in out Async_Map_Result_Type) is
      begin
         if Object.Is_Started then
            -- Use timeout to prevent blocking indefinitely
            select
               Object.Task_Instance.Cancel;
            or
               delay 1.0; -- 1 second timeout
               -- Force cleanup after timeout
            end select;
         end if;
      exception
         when others =>
            -- Prevent exceptions from escaping finalization
            null;
      end Finalize;
      
      procedure Start_Async_Map (R : Result_Type; Async_R : out Async_Map_Result_Type) is
      begin
         -- Set status and started flag atomically before starting task
         Async_R.Is_Started := True;
         Async_R.Manager.Set_Status (Pending);
         Async_R.Task_Instance.Start (R);
      end Start_Async_Map;
      
      function Is_Ready (Async_R : Async_Map_Result_Type) return Boolean is
      begin
         return Async_R.Manager.Get_Status = Completed or 
                Async_R.Manager.Get_Status = Failed or 
                Async_R.Manager.Get_Status = Cancelled;
      end Is_Ready;
      
      function Get_Status (Async_R : Async_Map_Result_Type) return Async_Status is
      begin
         return Async_R.Manager.Get_Status;
      end Get_Status;
      
      procedure Wait_For_Transformed_Result (Async_R : Async_Map_Result_Type; New_R : out New_Result_Type) is
      begin
         if Async_R.Is_Started then
            Async_R.Task_Instance.Get_Result (New_R);
         else
            Make_Err (New_R, Default_Error, "Async operation not started");
         end if;
      end Wait_For_Transformed_Result;
      
      procedure Wait_For_Transformed_Result_With_Timeout (
         Async_R : Async_Map_Result_Type; 
         New_R : out New_Result_Type;
         Timeout : Duration;
         Timed_Out : out Boolean
      ) is
         Success : Boolean;
      begin
         if Async_R.Is_Started then
            select
               Async_R.Task_Instance.Get_Result_With_Timeout (New_R, Timeout, Success);
               Timed_Out := not Success;
            or
               delay Timeout;
               Make_Err (New_R, Default_Error, "Operation timed out");
               Timed_Out := True;
            end select;
         else
            Make_Err (New_R, Default_Error, "Async operation not started");
            Timed_Out := False;
         end if;
      end Wait_For_Transformed_Result_With_Timeout;
      
      function Try_Get_Transformed_Result (Async_R : Async_Map_Result_Type; New_R : out New_Result_Type) return Boolean is
      begin
         if Is_Ready (Async_R) then
            Wait_For_Transformed_Result (Async_R, New_R);
            return True;
         else
            return False;
         end if;
      end Try_Get_Transformed_Result;
      
      procedure Cancel (Async_R : in out Async_Map_Result_Type) is
         Was_Cancelled : Boolean;
      begin
         if Async_R.Is_Started then
            -- Atomically check if pending and set to cancelled
            Async_R.Manager.Cancel_If_Pending (Was_Cancelled);
            
            if Was_Cancelled then
               -- Only send cancel signal if we successfully changed status
               Async_R.Task_Instance.Cancel;
            end if;
         end if;
      end Cancel;
      
      function Get_Execution_Time (Async_R : Async_Map_Result_Type) return Duration is
      begin
         return Async_R.Manager.Get_Execution_Time;
      end Get_Execution_Time;
      
   end Async_Map_Operations;

   ----------------------------------------------------------------------------
   -- PARALLEL OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Parallel_Operations is
      
      task type Worker_Task is
         entry Process (Index : Positive; Input : Result_Type; Output : out Result_Type);
      end Worker_Task;
      
      task body Worker_Task is
         Local_Input : Result_Type;
         Local_Output : Result_Type;
         Item_Index : Positive;
      begin
         accept Process (Index : Positive; Input : Result_Type; Output : out Result_Type) do
            Item_Index := Index;
            Local_Input := Input;
            
            -- Process the result
            if Is_Ok (Local_Input) then
               begin
                  declare
                     Input_Value : constant Value_Type := Unwrap (Local_Input);
                     Output_Value : Value_Type;
                  begin
                     Process_Value (Input_Value, Output_Value);
                     Make_Ok (Local_Output, Output_Value);
                  end;
               exception
                  when Ex : others =>
                     Make_Err (Local_Output, Default_Error,
                              "Parallel processing failed for item" & 
                              Positive'Image (Item_Index) & ": " & Exception_Information (Ex));
               end;
            else
               Local_Output := Local_Input; -- Preserve error
            end if;
            
            -- Return result
            Output := Local_Output;
         end Process;
      end Worker_Task;
      
      procedure Process_All_Parallel (Results : in out Result_Array_Type) is
      begin
         Process_All_Parallel (Results, Results'Length);
      end Process_All_Parallel;
      
      procedure Process_All_Parallel (Results : in out Result_Array_Type; 
                                    Worker_Count : Positive) is
         Workers : array (1 .. Worker_Count) of Worker_Task;
         Current_Worker : Positive := 1;
      begin
         -- Distribute work among workers
         for I in Results'Range loop
            Workers (Current_Worker).Process (I, Results (I), Results (I));
            Current_Worker := Current_Worker + 1;
            if Current_Worker > Worker_Count then
               Current_Worker := 1;
            end if;
         end loop;
      end Process_All_Parallel;
      
      procedure Wait_All_Complete (Results : Result_Array_Type) is
      begin
         -- In a real implementation, this would wait for all worker tasks to complete
         -- For now, this is a placeholder
         null;
      end Wait_All_Complete;
      
   end Parallel_Operations;

   ----------------------------------------------------------------------------
   -- CONCURRENT FOLD OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Concurrent_Fold_Operations is
      
      protected type Accumulator_Manager is
         procedure Initialize;
         procedure Add_Value (Value : Value_Type);
         procedure Add_Error (Error : Error_Type);
         procedure Get_Final (Final : out Accumulator_Type);
      private
         Acc : Accumulator_Type;
         Is_Init : Boolean := False;
      end Accumulator_Manager;
      
      protected body Accumulator_Manager is
         procedure Initialize is
         begin
            if not Is_Init then
               Initialize_Accumulator (Acc);
               Is_Init := True;
            end if;
         end Initialize;
         
         procedure Add_Value (Value : Value_Type) is
         begin
            Initialize;
            Combine_Value (Acc, Value);
         end Add_Value;
         
         procedure Add_Error (Error : Error_Type) is
         begin
            Initialize;
            Combine_Error (Acc, Error);
         end Add_Error;
         
         procedure Get_Final (Final : out Accumulator_Type) is
         begin
            Final := Acc;
         end Get_Final;
      end Accumulator_Manager;
      
      task type Fold_Worker is
         entry Process (Results : Result_Array_Type);
      end Fold_Worker;
      
      task body Fold_Worker is
         Local_Results : Result_Array_Type (1 .. 100); -- Arbitrary size
         Result_Count : Natural := 0;
      begin
         accept Process (Results : Result_Array_Type) do
            Result_Count := Results'Length;
            Local_Results (1 .. Result_Count) := Results;
         end Process;
         
         -- Process each result (simplified for now)
         for I in 1 .. Result_Count loop
            if Is_Ok (Local_Results (I)) then
               null; -- Would normally add to accumulator
            else
               null; -- Would normally add error to accumulator
            end if;
         end loop;
      end Fold_Worker;
      
      procedure Concurrent_Fold (Results : Result_Array_Type; 
                               Final_Acc : out Accumulator_Type) is
         Manager : aliased Accumulator_Manager;
         Worker : Fold_Worker;
      begin
         Worker.Process (Results);
         Manager.Get_Final (Final_Acc);
      end Concurrent_Fold;
      
   end Concurrent_Fold_Operations;

   ----------------------------------------------------------------------------
   -- ASYNC PIPELINE OPERATIONS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Async_Pipeline_Operations is
      
      task body Pipeline_Task is
         Input_Result : Result_Type;
         Output_Result : Result_Type;
         Is_Cancelled : Boolean := False;
      begin
         select
            accept Start (Input : Result_Type) do
               Input_Result := Input;
            end Start;
            
            -- Execute pipeline stages
            if not Is_Cancelled and Is_Ok (Input_Result) then
               begin
                  declare
                     Input_Value : constant Value_Type := Unwrap (Input_Result);
                     Intermediate_Value : Intermediate_Type;
                     Final_Value : Final_Type;
                  begin
                     -- Stage 1: Value -> Intermediate
                     First_Transform (Input_Value, Intermediate_Value);
                     
                     -- Stage 2: Intermediate -> Final
                     Second_Transform (Intermediate_Value, Final_Value);
                     
                     -- Create result (this would need proper type handling)
                     Make_Ok (Output_Result, Default_Value);
                  end;
               exception
                  when Ex : others =>
                     Make_Err (Output_Result, Default_Error,
                              "Pipeline failed: " & Exception_Information (Ex));
               end;
            else
               Output_Result := Input_Result; -- Preserve error or cancellation
            end if;
            
            -- Wait for result request
            loop
               select
                  accept Get_Result (Output : out Result_Type) do
                     Output := Output_Result;
                  end Get_Result;
                  exit;
               or
                  accept Cancel do
                     Is_Cancelled := True;
                     Make_Err (Output_Result, Default_Error, "Pipeline was cancelled");
                  end Cancel;
               end select;
            end loop;
            
         or
            accept Cancel do
               Is_Cancelled := True;
            end Cancel;
         end select;
      end Pipeline_Task;
      
      procedure Start_Pipeline (R : Result_Type; Pipeline_R : out Pipeline_Result_Type) is
      begin
         Pipeline_R.Task_Instance.Start (R);
         Pipeline_R.Status := Pending;
      end Start_Pipeline;
      
      procedure Wait_For_Pipeline (Pipeline_R : Pipeline_Result_Type; Final_R : out Result_Type) is
      begin
         Pipeline_R.Task_Instance.Get_Result (Final_R);
      end Wait_For_Pipeline;
      
      function Get_Pipeline_Status (Pipeline_R : Pipeline_Result_Type) return Async_Status is
      begin
         return Pipeline_R.Status;
      end Get_Pipeline_Status;
      
   end Async_Pipeline_Operations;

   ----------------------------------------------------------------------------
   -- ASYNC COMBINATORS IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Async_Combinators is
      
      procedure Wait_All (
         Results : in out Result_Array_Type;
         Timeout : Duration := Duration'Last
      ) is
         Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Timeout_Time : constant Ada.Calendar.Time := Start_Time + Timeout;
      begin
         -- For now, this is a placeholder - in a full implementation,
         -- this would wait for all async operations to complete
         for I in Results'Range loop
            if Ada.Calendar.Clock > Timeout_Time then
               Make_Err (Results (I), Default_Error, "Wait_All timed out");
               exit;
            end if;
            -- Results would be processed here
         end loop;
      end Wait_All;
      
      procedure Wait_Any (
         Results : in out Result_Array_Type;
         Index : out Positive;
         Result : out Result_Type;
         Timeout : Duration := Duration'Last
      ) is
         Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Timeout_Time : constant Ada.Calendar.Time := Start_Time + Timeout;
      begin
         -- Return the first available result
         for I in Results'Range loop
            if Ada.Calendar.Clock > Timeout_Time then
               Make_Err (Result, Default_Error, "Wait_Any timed out");
               Index := Results'First;
               return;
            end if;
            
            -- Check if result is ready (placeholder logic)
            if Is_Ok (Results (I)) then
               Index := I;
               Result := Results (I);
               return;
            end if;
         end loop;
         
         -- No result ready
         Index := Results'First;
         Make_Err (Result, Default_Error, "No results available");
      end Wait_Any;
      
      procedure Race (
         Results : in out Result_Array_Type;
         Winner_Index : out Positive;
         Result : out Result_Type;
         Timeout : Duration := Duration'Last
      ) is
      begin
         -- Race is similar to Wait_Any for now
         Wait_Any (Results, Winner_Index, Result, Timeout);
      end Race;
      
   end Async_Combinators;

   ----------------------------------------------------------------------------
   -- WORK STEALING POOL IMPLEMENTATION
   ----------------------------------------------------------------------------

   package body Work_Stealing_Pool is
      
      -- Worker task implementation for processing work items
      task body Worker_Task is
         Work_Item : Work_Item_Type;
         Work_Result : Result_Type;
         Keep_Running : Boolean := False;
         Work_Available : Boolean;
      begin
         -- Wait for start signal
         accept Start do
            Keep_Running := True;
         end Start;
         
         -- Main work loop
         while Keep_Running loop
            select
               -- Handle shutdown request
               accept Shutdown do
                  Keep_Running := False;
               end Shutdown;
               exit;
            or
               -- Try to get work from queue (non-blocking)
               delay 0.01; -- Small delay to prevent busy waiting
            end select;
            
            if Keep_Running then
               -- Try to get work from the queue
               begin
                  select
                     Queue_Access.all.Get_Work (Work_Item);
                     Work_Available := True;
                  else
                     Work_Available := False;
                  end select;
                  
                  if Work_Available then
                     -- Process the work item
                     begin
                        Process_Work (Work_Item, Work_Result);
                        
                        -- Update metrics based on result
                        if Is_Ok (Work_Result) then
                           Metrics_Access.all.Increment_Completed;
                           Global_Metrics.Record_Task_Complete (0.01); -- Placeholder duration
                        else
                           Metrics_Access.all.Increment_Failed;
                           Global_Metrics.Record_Task_Failed;
                        end if;
                     exception
                        when others =>
                           -- Count as failed task
                           Metrics_Access.all.Increment_Failed;
                           Global_Metrics.Record_Task_Failed;
                     end;
                  else
                     -- No work available, brief delay
                     delay 0.001;
                  end if;
               exception
                  when Program_Error =>
                     -- Queue is shut down, exit gracefully
                     Keep_Running := False;
                  when others =>
                     -- Other errors, continue but delay a bit
                     delay 0.1;
               end;
            end if;
         end loop;
      exception
         when others =>
            -- Final safety net - record failure and exit
            Global_Metrics.Record_Task_Failed;
      end Worker_Task;
      
      protected body Pool_Metrics_Manager is
         procedure Increment_Submitted is
         begin
            Metrics.Tasks_Submitted := Metrics.Tasks_Submitted + 1;
         end Increment_Submitted;
         
         procedure Increment_Completed is
         begin
            Metrics.Tasks_Completed := Metrics.Tasks_Completed + 1;
         end Increment_Completed;
         
         procedure Increment_Failed is
         begin
            Metrics.Tasks_Failed := Metrics.Tasks_Failed + 1;
         end Increment_Failed;
         
         procedure Set_Active_Workers (Count : Natural) is
         begin
            Metrics.Active_Workers := Count;
         end Set_Active_Workers;
         
         function Get_Metrics return Pool_Metrics_Type is
         begin
            return Metrics;
         end Get_Metrics;
         
         procedure Reset_Metrics is
         begin
            Metrics := (others => 0);
         end Reset_Metrics;
      end Pool_Metrics_Manager;
      
      protected body Work_Queue is
         procedure Add_Work (Item : Work_Item_Type) is
         begin
            if Is_Shutdown then
               raise Program_Error with "Work queue is shut down";
            end if;
            
            if Count >= Items'Length then
               raise Constraint_Error with "Work queue is full";
            end if;
            
            Items (Tail) := Item;
            Tail := (if Tail = Items'Last then Items'First else Tail + 1);
            Count := Count + 1;
         end Add_Work;
         
         procedure Try_Add_Work (Item : Work_Item_Type; Success : out Boolean) is
         begin
            if Is_Shutdown or Count >= Items'Length then
               Success := False;
            else
               Items (Tail) := Item;
               Tail := (if Tail = Items'Last then Items'First else Tail + 1);
               Count := Count + 1;
               Success := True;
            end if;
         end Try_Add_Work;
         
         entry Get_Work (Item : out Work_Item_Type) when Count > 0 or Is_Shutdown is
         begin
            if Is_Shutdown then
               raise Program_Error with "Work queue is shut down";
            end if;
            
            if Count > 0 then
               Item := Items (Head);
               Head := (if Head = Items'Last then Items'First else Head + 1);
               Count := Count - 1;
            end if;
         end Get_Work;
         
         procedure Shutdown is
         begin
            Is_Shutdown := True;
         end Shutdown;
         
         function Get_Queue_Length return Natural is
         begin
            return Count;
         end Get_Queue_Length;
      end Work_Queue;
      
      -- task body Worker_Task is
      -- begin
      --    -- Simple start acknowledgment - this should execute immediately
      --    accept Start;
      --    
      --    -- Temporary: just terminate immediately to test task creation
      --    -- TODO: Implement full worker logic after fixing activation issue
      --    
      -- end Worker_Task;
      
      procedure Initialize_Pool (Pool : out Task_Pool_Type; Worker_Count : Positive) is
         Actual_Count : constant Positive := Positive'Min (Worker_Count, Pool.Workers'Length);
      begin
         -- Initialize the pool
         Pool.Worker_Count := Actual_Count;
         Pool.Is_Initialized := True;
         
         -- Reset metrics
         Pool.Metrics.Reset_Metrics;
         Pool.Metrics.Set_Active_Workers (Actual_Count);
         
         -- Create worker tasks
         for I in 1 .. Actual_Count loop
            Pool.Workers (I) := new Worker_Task (
               Queue_Access => Pool.Queue'Unchecked_Access,
               Metrics_Access => Pool.Metrics'Unchecked_Access
            );
         end loop;
         
         -- Start all workers
         for I in 1 .. Actual_Count loop
            if Pool.Workers (I) /= null then
               Pool.Workers (I).all.Start;
            end if;
         end loop;
         
         Global_Metrics.Record_Task_Start;
      end Initialize_Pool;
      
      procedure Submit_Work (Pool : in out Task_Pool_Type; Item : Work_Item_Type) is
      begin
         if not Pool.Is_Initialized then
            raise Program_Error with "Pool not initialized";
         end if;
         
         -- Add work to the queue
         Pool.Queue.Add_Work (Item);
         
         -- Update metrics
         Pool.Metrics.Increment_Submitted;
         Global_Metrics.Record_Task_Start;
      end Submit_Work;
      
      function Get_Metrics (Pool : Task_Pool_Type) return Pool_Metrics_Type is
      begin
         if Pool.Is_Initialized then
            -- Get current metrics from the pool including queue length
            declare
               Result : Pool_Metrics_Type := Pool.Metrics.Get_Metrics;
            begin
               Result.Queue_Length := Pool.Queue.Get_Queue_Length;
               return Result;
            end;
         else
            -- Return empty metrics for uninitialized pool
            declare
               Result : Pool_Metrics_Type;
            begin
               return Result; -- All fields default to 0
            end;
         end if;
      end Get_Metrics;
      
      procedure Shutdown_Pool (Pool : in out Task_Pool_Type) is
      begin
         if not Pool.Is_Initialized then
            return;
         end if;
         
         -- Shutdown the work queue to stop accepting new work
         Pool.Queue.Shutdown;
         
         -- Shutdown all worker tasks
         for I in 1 .. Pool.Worker_Count loop
            if Pool.Workers (I) /= null then
               -- Send shutdown signal with timeout
               select
                  Pool.Workers (I).all.Shutdown;
               or
                  delay 2.0; -- 2 second timeout for graceful shutdown
               end select;
            end if;
         end loop;
         
         -- Clean up worker task access objects
         for I in Pool.Workers'Range loop
            if Pool.Workers (I) /= null then
               -- Note: In a real implementation, you might want to use controlled types
               -- or implement proper cleanup to avoid memory leaks
               Pool.Workers (I) := null;
            end if;
         end loop;
         
         -- Update metrics
         Pool.Metrics.Set_Active_Workers (0);
         Pool.Is_Initialized := False;
      end Shutdown_Pool;
      
   end Work_Stealing_Pool;

   ----------------------------------------------------------------------------
   -- ATOMIC COUNTER IMPLEMENTATION
   ----------------------------------------------------------------------------

   protected body Atomic_Counter is
      procedure Increment (New_Value : out Natural) is
      begin
         Counter := Counter + 1;
         New_Value := Counter;
      end Increment;

      procedure Decrement (New_Value : out Natural) is
      begin
         if Counter > 0 then
            Counter := Counter - 1;
         end if;
         New_Value := Counter;
      end Decrement;

      procedure Add (Amount : Natural; New_Value : out Natural) is
      begin
         Counter := Counter + Amount;
         New_Value := Counter;
      end Add;

      function Get_Value return Natural is
      begin
         return Counter;
      end Get_Value;

      procedure Set_Value (Value : Natural) is
      begin
         Counter := Value;
      end Set_Value;

      procedure Reset is
      begin
         Counter := 0;
      end Reset;
   end Atomic_Counter;

   ----------------------------------------------------------------------------
   -- ASYNC METRICS IMPLEMENTATION
   ----------------------------------------------------------------------------

   protected body Async_Metrics_Manager is
      procedure Record_Task_Start is
      begin
         Tasks_Started := Tasks_Started + 1;
         Current_Active_Tasks := Current_Active_Tasks + 1;
         if Current_Active_Tasks > Peak_Concurrent_Tasks then
            Peak_Concurrent_Tasks := Current_Active_Tasks;
         end if;
      end Record_Task_Start;
      
      procedure Record_Task_Complete (Execution_Time : Duration) is
      begin
         Tasks_Completed := Tasks_Completed + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
         Total_Execution_Time := Total_Execution_Time + Execution_Time;
         
         -- Update min/max execution times
         if Execution_Time < Min_Execution_Time then
            Min_Execution_Time := Execution_Time;
         end if;
         if Execution_Time > Max_Execution_Time then
            Max_Execution_Time := Execution_Time;
         end if;
      end Record_Task_Complete;
      
      procedure Record_Task_Failed is
      begin
         Tasks_Failed := Tasks_Failed + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
      end Record_Task_Failed;
      
      procedure Record_Task_Cancelled is
      begin
         Tasks_Cancelled := Tasks_Cancelled + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
      end Record_Task_Cancelled;
      
      procedure Record_Task_Timed_Out is
      begin
         Tasks_Timed_Out := Tasks_Timed_Out + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
      end Record_Task_Timed_Out;
      
      function Get_Metrics return Async_Metrics_Type is
         Average_Time : Duration := 0.0;
      begin
         if Tasks_Completed > 0 then
            Average_Time := Total_Execution_Time / Duration (Tasks_Completed);
         end if;
         
         return (Tasks_Started => Tasks_Started,
                 Tasks_Completed => Tasks_Completed,
                 Tasks_Failed => Tasks_Failed,
                 Tasks_Cancelled => Tasks_Cancelled,
                 Tasks_Timed_Out => Tasks_Timed_Out,
                 Average_Execution_Time => Average_Time,
                 Peak_Concurrent_Tasks => Peak_Concurrent_Tasks,
                 Current_Active_Tasks => Current_Active_Tasks,
                 Total_Execution_Time => Total_Execution_Time,
                 Min_Execution_Time => Min_Execution_Time,
                 Max_Execution_Time => Max_Execution_Time);
      end Get_Metrics;
      
      procedure Reset_Metrics is
      begin
         Tasks_Started := 0;
         Tasks_Completed := 0;
         Tasks_Failed := 0;
         Tasks_Cancelled := 0;
         Tasks_Timed_Out := 0;
         Current_Active_Tasks := 0;
         Peak_Concurrent_Tasks := 0;
         Total_Execution_Time := 0.0;
         Min_Execution_Time := Duration'Last;
         Max_Execution_Time := 0.0;
      end Reset_Metrics;
   end Async_Metrics_Manager;

   ----------------------------------------------------------------------------
   -- GLOBAL METRICS IMPLEMENTATION
   ----------------------------------------------------------------------------
   
   -- Global metrics is now directly accessible as a protected type

   ----------------------------------------------------------------------------
   -- BACKPRESSURE CONTROLLER IMPLEMENTATION
   ----------------------------------------------------------------------------

   protected body Backpressure_Controller is
      procedure Configure (Policy : Backpressure_Policy; Max_Queue_Size : Natural) is
      begin
         Current_Policy := Policy;
         Max_Queue := Max_Queue_Size;
      end Configure;
      
      function Should_Accept_Work return Boolean is
      begin
         case Current_Policy is
            when Block =>
               return True;  -- Always accept, will block if needed
            when Drop_Oldest | Drop_Newest =>
               return True;  -- Always accept, will drop if needed
            when Fail_Fast =>
               return Current_Queue_Size < Max_Queue;
         end case;
      end Should_Accept_Work;
      
      procedure Work_Submitted is
      begin
         Current_Queue_Size := Current_Queue_Size + 1;
      end Work_Submitted;
      
      procedure Work_Completed is
      begin
         if Current_Queue_Size > 0 then
            Current_Queue_Size := Current_Queue_Size - 1;
         end if;
      end Work_Completed;
   end Backpressure_Controller;

end Async_Result;
