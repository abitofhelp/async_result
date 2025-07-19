-- =============================================================================
-- Ada Async_Result Library - Prelude Body
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Implementation of pre-instantiated types and helpers
-- =============================================================================

package body Async_Result.Prelude is

   ----------------------------------------------------------------------------
   -- Convenience Functions
   ----------------------------------------------------------------------------

   function Make_Error (
      Kind : Error_Kind; 
      Message : String := "";
      Code : Integer := 0
   ) return Error_Info is
   begin
      return (Kind => Kind,
              Message => (if Message = "" then Null_Unbounded_String 
                         else To_Unbounded_String (Message)),
              Code => Code);
   end Make_Error;

   function Error_To_String (E : Error_Info) return String is
      Kind_Str : constant String := Error_Kind'Image (E.Kind);
      Msg_Str : constant String := To_String (E.Message);
   begin
      if Length (E.Message) > 0 then
         return Kind_Str & ": " & Msg_Str & " (Code: " & Integer'Image (E.Code) & ")";
      else
         return Kind_Str & " (Code: " & Integer'Image (E.Code) & ")";
      end if;
   end Error_To_String;

   function Invalid_Argument_Error (Message : String := "") 
                                  return Integer_Result.Result_Type is
      R : Integer_Result.Result_Type;
   begin
      Integer_Result.Make_Err (R, Make_Error (Invalid_Argument, Message));
      return R;
   end Invalid_Argument_Error;

   function Timeout_Error (Message : String := "") 
                         return Integer_Result.Result_Type is
      R : Integer_Result.Result_Type;
   begin
      Integer_Result.Make_Err (R, Make_Error (Timeout_Error, Message));
      return R;
   end Timeout_Error;

   function IO_Error (Message : String := "") 
                    return String_Result.Result_Type is
      R : String_Result.Result_Type;
   begin
      String_Result.Make_Err (R, Make_Error (IO_Error, Message));
      return R;
   end IO_Error;

end Async_Result.Prelude;