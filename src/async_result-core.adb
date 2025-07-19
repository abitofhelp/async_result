-- =============================================================================
-- Ada Async_Result Library - Core Result Types Body
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Implementation of core Result types and operations
-- =============================================================================

with Ada.Exceptions; use Ada.Exceptions;

package body Async_Result.Core is

   -- Targeted pragma suppressions for performance-critical sections
   pragma Suppress (Index_Check, Get_State);
   pragma Suppress (Index_Check, Is_Ok);
   pragma Suppress (Index_Check, Is_Err);

   ----------------------------------------------------------------------------
   -- Internal Validation
   ----------------------------------------------------------------------------

   procedure Ensure_Valid_State (R : Result_Type) is
   begin
      if not R.Is_Initialized then
         raise Result_Not_Valid with "Result type not properly initialized";
      end if;
   end Ensure_Valid_State;

   ----------------------------------------------------------------------------
   -- Memory Management (RAII Pattern)
   ----------------------------------------------------------------------------

   overriding procedure Initialize (Object : in out Result_Type) is
   begin
      -- Initialize Result in error state with default values
      Object.State := Error;
      Object.Value := Default_Value;
      Object.Error := Default_Error;
      Object.Error_Message := Null_Unbounded_String;
      Object.Is_Initialized := True;
   end Initialize;

   overriding procedure Adjust (Object : in out Result_Type) is
   begin
      -- Handle deep copying when Result is copied
      case Object.State is
         when Success =>
            begin
               Object.Value := Copy_Value (Object.Value);
            exception
               when others =>
                  -- If copying fails, convert to error state
                  Object.State := Error;
                  Object.Error := Default_Error;
                  Object.Error_Message := To_Unbounded_String ("Value copy failed during adjust");
            end;
         when Error =>
            begin
               Object.Error := Copy_Error (Object.Error);
            exception
               when others =>
                  -- If error copying fails, use default error
                  Object.Error := Default_Error;
                  Object.Error_Message := To_Unbounded_String ("Error copy failed during adjust");
            end;
      end case;
      Object.Is_Initialized := True;
   end Adjust;

   overriding procedure Finalize (Object : in out Result_Type) is
   begin
      -- Clean up resources when Result goes out of scope
      if Object.Is_Initialized then
         Object.Is_Initialized := False;
         Object.Error_Message := Null_Unbounded_String;
      end if;
   exception
      when others =>
         -- Finalize must never propagate exceptions
         Object.Is_Initialized := False;
   end Finalize;

   ----------------------------------------------------------------------------
   -- Construction Operations
   ----------------------------------------------------------------------------

   procedure Make_Ok (R : out Result_Type; Value : Value_Type) is
   begin
      R.State := Success;
      R.Value := Copy_Value (Value);
      R.Error := Default_Error;
      R.Error_Message := Null_Unbounded_String;
      R.Is_Initialized := True;
   end Make_Ok;

   procedure Make_Err (R : out Result_Type; Err : Error_Type; Message : String := "") is
   begin
      R.State := Error;
      R.Value := Default_Value;
      R.Error := Copy_Error (Err);
      if Message = "" then
         R.Error_Message := Null_Unbounded_String;
      else
         R.Error_Message := To_Unbounded_String (Message);
      end if;
      R.Is_Initialized := True;
   end Make_Err;

   ----------------------------------------------------------------------------
   -- Query Operations
   ----------------------------------------------------------------------------

   function Is_Ok (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return R.State = Success;
   end Is_Ok;

   function Is_Err (R : Result_Type) return Boolean is
   begin
      Ensure_Valid_State (R);
      return R.State = Error;
   end Is_Err;

   function Get_State (R : Result_Type) return Result_State is
   begin
      Ensure_Valid_State (R);
      return R.State;
   end Get_State;

   ----------------------------------------------------------------------------
   -- Extraction Operations
   ----------------------------------------------------------------------------

   function Unwrap (R : Result_Type) return Value_Type is
   begin
      if not Is_Ok (R) then
         if Length (R.Error_Message) > 0 then
            raise Unwrap_Error with "Called unwrap on error result: " & To_String (R.Error_Message);
         else
            raise Unwrap_Error with "Called unwrap on error result";
         end if;
      end if;
      return R.Value;
   end Unwrap;

   function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type is
   begin
      if Is_Ok (R) then
         return R.Value;
      else
         return Default;
      end if;
   end Unwrap_Or;

   function Unwrap_Err (R : Result_Type) return Error_Type is
   begin
      if not Is_Err (R) then
         raise Unwrap_Error with "Called unwrap_err on success result";
      end if;
      return R.Error;
   end Unwrap_Err;

   function Get_Error_Message (R : Result_Type) return String is
   begin
      if not Is_Err (R) then
         raise Result_Not_Valid with "Called get_error_message on success result";
      end if;
      return To_String (R.Error_Message);
   end Get_Error_Message;

   ----------------------------------------------------------------------------
   -- Safe Extraction Operations
   ----------------------------------------------------------------------------

   function Try_Get_Value (R : Result_Type; Value : out Value_Type) return Boolean is
   begin
      if Is_Ok (R) then
         Value := Copy_Value (R.Value);
         return True;
      else
         Value := Default_Value;
         return False;
      end if;
   end Try_Get_Value;

   function Try_Get_Error (R : Result_Type; Err : out Error_Type) return Boolean is
   begin
      if Is_Err (R) then
         Err := Copy_Error (R.Error);
         return True;
      else
         Err := Default_Error;
         return False;
      end if;
   end Try_Get_Error;

   function Try_Get_Message (R : Result_Type; Message : out Unbounded_String) return Boolean is
   begin
      if Is_Err (R) and then Length (R.Error_Message) > 0 then
         Message := R.Error_Message;
         return True;
      else
         Message := Null_Unbounded_String;
         return False;
      end if;
   end Try_Get_Message;

   ----------------------------------------------------------------------------
   -- Pattern Matching
   ----------------------------------------------------------------------------

   procedure Match (R : Result_Type) is
   begin
      Ensure_Valid_State (R);
      case R.State is
         when Success =>
            Handle_Ok (R.Value);
         when Error =>
            Handle_Err (R.Error, To_String (R.Error_Message));
      end case;
   end Match;

   ----------------------------------------------------------------------------
   -- Map Operations Implementation
   ----------------------------------------------------------------------------

   package body Map_Operations is
      
      overriding procedure Initialize (Object : in out New_Result_Type) is
      begin
         Object.State := Error;
         Object.Value := Default_New_Value;
         Object.Error := Default_Error;
         Object.Error_Message := Null_Unbounded_String;
         Object.Is_Initialized := True;
      end Initialize;

      overriding procedure Adjust (Object : in out New_Result_Type) is
      begin
         case Object.State is
            when Success =>
               begin
                  Object.Value := Copy_New_Value (Object.Value);
               exception
                  when others =>
                     Object.State := Error;
                     Object.Error := Default_Error;
                     Object.Error_Message := To_Unbounded_String ("Value copy failed during adjust");
               end;
            when Error =>
               begin
                  Object.Error := Copy_Error (Object.Error);
               exception
                  when others =>
                     Object.Error := Default_Error;
                     Object.Error_Message := To_Unbounded_String ("Error copy failed during adjust");
               end;
         end case;
         Object.Is_Initialized := True;
      end Adjust;

      overriding procedure Finalize (Object : in out New_Result_Type) is
      begin
         if Object.Is_Initialized then
            Object.Is_Initialized := False;
            Object.Error_Message := Null_Unbounded_String;
         end if;
      exception
         when others =>
            Object.Is_Initialized := False;
      end Finalize;

      procedure Map (R : Result_Type; New_R : out New_Result_Type) is
      begin
         if Is_Ok (R) then
            declare
               New_Value : New_Value_Type;
            begin
               Transform (R.Value, New_Value);
               Make_Ok (New_R, New_Value);
            exception
               when E : others =>
                  Make_Err (New_R, Default_Error, "Transform failed: " & Exception_Information (E));
            end;
         else
            Make_Err (New_R, R.Error, To_String (R.Error_Message));
         end if;
      end Map;

      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type) is
      begin
         R.State := Success;
         R.Value := Copy_New_Value (Value);
         R.Error := Default_Error;
         R.Error_Message := Null_Unbounded_String;
         R.Is_Initialized := True;
      end Make_Ok;

      procedure Make_Err (R : out New_Result_Type; Err : Error_Type; Message : String := "") is
      begin
         R.State := Error;
         R.Value := Default_New_Value;
         R.Error := Copy_Error (Err);
         if Message = "" then
            R.Error_Message := Null_Unbounded_String;
         else
            R.Error_Message := To_Unbounded_String (Message);
         end if;
         R.Is_Initialized := True;
      end Make_Err;

      function Is_Ok (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Success;
      end Is_Ok;

      function Is_Err (R : New_Result_Type) return Boolean is
      begin
         return R.Is_Initialized and then R.State = Error;
      end Is_Err;

      function Get_State (R : New_Result_Type) return Result_State is
      begin
         if not R.Is_Initialized then
            raise Result_Not_Valid with "Result not initialized";
         end if;
         return R.State;
      end Get_State;

      function Unwrap (R : New_Result_Type) return New_Value_Type is
      begin
         if not Is_Ok (R) then
            if Length (R.Error_Message) > 0 then
               raise Unwrap_Error with "Called unwrap on error result: " & To_String (R.Error_Message);
            else
               raise Unwrap_Error with "Called unwrap on error result";
            end if;
         end if;
         return R.Value;
      end Unwrap;

      function Unwrap_Err (R : New_Result_Type) return Error_Type is
      begin
         if not Is_Err (R) then
            raise Unwrap_Error with "Called unwrap_err on success result";
         end if;
         return R.Error;
      end Unwrap_Err;

      function Get_Error_Message (R : New_Result_Type) return String is
      begin
         if not Is_Err (R) then
            raise Result_Not_Valid with "Called get_error_message on success result";
         end if;
         return To_String (R.Error_Message);
      end Get_Error_Message;

   end Map_Operations;

   ----------------------------------------------------------------------------
   -- And_Then Operations Implementation
   ----------------------------------------------------------------------------

   package body And_Then_Operations is
      
      procedure And_Then (R : Result_Type; New_R : out Result_Type) is
      begin
         if Is_Ok (R) then
            begin
               Transform (R.Value, New_R);
            exception
               when E : others =>
                  Make_Err (New_R, Default_Error, "And_Then transform failed: " & Exception_Information (E));
            end;
         else
            -- Propagate the error
            New_R := R;
         end if;
      end And_Then;

   end And_Then_Operations;

   ----------------------------------------------------------------------------
   -- Protected Result Type Implementation
   ----------------------------------------------------------------------------

   protected body Protected_Result_Type is
      
      procedure Make_Ok (Value : Value_Type) is
      begin
         Core.Make_Ok (Result, Value);
         Is_Set := True;
      end Make_Ok;

      procedure Make_Err (Err : Error_Type; Message : String := "") is
      begin
         Core.Make_Err (Result, Err, Message);
         Is_Set := True;
      end Make_Err;

      function Is_Ok return Boolean is
      begin
         return Is_Set and then Core.Is_Ok (Result);
      end Is_Ok;

      function Is_Err return Boolean is
      begin
         return Is_Set and then Core.Is_Err (Result);
      end Is_Err;

      function Get_State return Result_State is
      begin
         if not Is_Set then
            return Error;
         end if;
         return Core.Get_State (Result);
      end Get_State;

      function Try_Get_Value (Value : out Value_Type) return Boolean is
      begin
         if Is_Set then
            return Core.Try_Get_Value (Result, Value);
         else
            Value := Default_Value;
            return False;
         end if;
      end Try_Get_Value;

      function Try_Get_Error (Err : out Error_Type; Message : out Unbounded_String) return Boolean is
      begin
         if Is_Set and then Core.Is_Err (Result) then
            Err := Result.Error;
            Message := Result.Error_Message;
            return True;
         else
            Err := Default_Error;
            Message := Null_Unbounded_String;
            return False;
         end if;
      end Try_Get_Error;

      entry Wait_For_Result (R : out Result_Type) when Is_Set is
      begin
         R := Result;
      end Wait_For_Result;

   end Protected_Result_Type;

   ----------------------------------------------------------------------------
   -- Utility Functions
   ----------------------------------------------------------------------------

   function To_Result (Value : Value_Type) return Result_Type is
      R : Result_Type;
   begin
      Make_Ok (R, Value);
      return R;
   end To_Result;

   function To_Error_Result (Err : Error_Type; Message : String := "") return Result_Type is
      R : Result_Type;
   begin
      Make_Err (R, Err, Message);
      return R;
   end To_Error_Result;

   function To_String (R : Result_Type) return String is
   begin
      if Is_Ok (R) then
         return "Ok(<value>)";
      else
         if Length (R.Error_Message) > 0 then
            return "Err(<error>: " & To_String (R.Error_Message) & ")";
         else
            return "Err(<error>)";
         end if;
      end if;
   end To_String;

end Async_Result.Core;