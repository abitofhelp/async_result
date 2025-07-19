-- =============================================================================
-- Ada Async_Result Library - Asynchronous Operations Body
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Implementation of asynchronous operations and futures
-- =============================================================================

package body Async_Result.Async is

   ----------------------------------------------------------------------------
   -- Cancellation Token Implementation
   ----------------------------------------------------------------------------

   protected body Cancellation_Token is
      
      procedure Request_Cancellation is
      begin
         Cancelled := True;
      end Request_Cancellation;
      
      function Is_Cancelled return Boolean is
      begin
         return Cancelled;
      end Is_Cancelled;
      
      procedure Reset is
      begin
         Cancelled := False;
      end Reset;
      
   end Cancellation_Token;

   ----------------------------------------------------------------------------
   -- Global Functions
   ----------------------------------------------------------------------------

   function Should_Continue_Operation return Boolean is
   begin
      return not Global_Cancellation.Is_Cancelled;
   end Should_Continue_Operation;

   ----------------------------------------------------------------------------
   -- Timeout Configuration
   ----------------------------------------------------------------------------

   procedure Set_Default_Operation_Timeout (Timeout : Duration) is
   begin
      Global_Timeout_Config.Default_Operation_Timeout := Timeout;
   end Set_Default_Operation_Timeout;

   procedure Set_Task_Startup_Timeout (Timeout : Duration) is
   begin
      Global_Timeout_Config.Task_Startup_Timeout := Timeout;
   end Set_Task_Startup_Timeout;

   procedure Set_Task_Shutdown_Timeout (Timeout : Duration) is
   begin
      Global_Timeout_Config.Task_Shutdown_Timeout := Timeout;
   end Set_Task_Shutdown_Timeout;

   procedure Set_Result_Retrieval_Timeout (Timeout : Duration) is
   begin
      Global_Timeout_Config.Result_Retrieval_Timeout := Timeout;
   end Set_Result_Retrieval_Timeout;

   function Get_Default_Operation_Timeout return Duration is
   begin
      return Global_Timeout_Config.Default_Operation_Timeout;
   end Get_Default_Operation_Timeout;

   function Get_Task_Startup_Timeout return Duration is
   begin
      return Global_Timeout_Config.Task_Startup_Timeout;
   end Get_Task_Startup_Timeout;

   function Get_Task_Shutdown_Timeout return Duration is
   begin
      return Global_Timeout_Config.Task_Shutdown_Timeout;
   end Get_Task_Shutdown_Timeout;

   function Get_Result_Retrieval_Timeout return Duration is
   begin
      return Global_Timeout_Config.Result_Retrieval_Timeout;
   end Get_Result_Retrieval_Timeout;

   ----------------------------------------------------------------------------
   -- Async Computation Task Body
   ----------------------------------------------------------------------------

   task body Async_Computation_Task is
      Input_Value : Core.Value_Type;
      Result_Value : Core.Result_Type;
      Should_Shutdown : Boolean := False;
   begin
      loop
         select
            accept Start (Input : Core.Value_Type) do
               Input_Value := Input;
            end Start;
            
            -- Perform computation here
            Core.Make_Ok (Result_Value, Input_Value);
            
         or
            accept Get_Result (R : out Core.Result_Type) do
               R := Result_Value;
            end Get_Result;
            
         or
            accept Cancel;
            Core.Make_Err (Result_Value, Core.Default_Error, "Operation cancelled");
            
         or
            accept Shutdown;
            Should_Shutdown := True;
            
         or
            terminate;
         end select;
         
         exit when Should_Shutdown;
      end loop;
   end Async_Computation_Task;

end Async_Result.Async;