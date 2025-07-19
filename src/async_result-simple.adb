-- =============================================================================
-- Ada Async_Result Library - Simplified API Body
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Implementation of simplified API for common use cases
-- =============================================================================

package body Async_Result.Simple is

   ----------------------------------------------------------------------------
   -- Creation Helpers
   ----------------------------------------------------------------------------

   function Ok (Value : Value_Type) return Result is
      R : Result;
   begin
      Core.Make_Ok (R, Value);
      return R;
   end Ok;

   function Err (Error : Error_Type) return Result is
      R : Result;
   begin
      Core.Make_Err (R, Error);
      return R;
   end Err;

   function Err (Error : Error_Type; Message : String) return Result is
      R : Result;
   begin
      Core.Make_Err (R, Error, Message);
      return R;
   end Err;

   ----------------------------------------------------------------------------
   -- Simple Error Helpers
   ----------------------------------------------------------------------------

   function Err (Error : Simple_Error; Message : String := "") return Result is
      R : Result;
      E : Error_Type;
   begin
      -- This would need to be adapted based on actual Error_Type
      -- For now, assuming Error_Type can be constructed from Simple_Error
      Core.Make_Err (R, E, Message);
      return R;
   end Err;

   ----------------------------------------------------------------------------
   -- Common Patterns
   ----------------------------------------------------------------------------

   function All_Ok (Results : Result_Array) return Boolean is
   begin
      for R of Results loop
         if not Core.Is_Ok (R) then
            return False;
         end if;
      end loop;
      return True;
   end All_Ok;

   function Any_Ok (Results : Result_Array) return Boolean is
   begin
      for R of Results loop
         if Core.Is_Ok (R) then
            return True;
         end if;
      end loop;
      return False;
   end Any_Ok;

   function First_Ok (Results : Result_Array) return Result is
   begin
      for R of Results loop
         if Core.Is_Ok (R) then
            return R;
         end if;
      end loop;
      -- Return last element if none are Ok
      if Results'Length > 0 then
         return Results (Results'Last);
      else
         return Err (Default_Error_Default);
      end if;
   end First_Ok;

   function First_Err (Results : Result_Array) return Result is
   begin
      for R of Results loop
         if Core.Is_Err (R) then
            return R;
         end if;
      end loop;
      -- Return last element if none are Err
      if Results'Length > 0 then
         return Results (Results'Last);
      else
         return Ok (Default_Value_Default);
      end if;
   end First_Err;

end Async_Result.Simple;