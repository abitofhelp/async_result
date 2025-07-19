-- =============================================================================
-- Ada Async_Result Library - Asynchronous Operations
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Asynchronous operations and futures for Result types
-- Provides concurrent transformations and async combinators
-- =============================================================================

with Ada.Finalization;
with Ada.Calendar;
with Ada.Task_Identification;
with Ada.Strings.Unbounded;

generic
   -- Import the core types from parent instantiation
   with package Core is new Async_Result.Core (<>);
   use Core;
package Async_Result.Async is
   pragma Elaborate_Body;

   use Ada.Strings.Unbounded;

   -- ==========================================================================
   -- Async Status Types
   -- ==========================================================================
   
   -- Status of asynchronous operations
   type Async_Status is (Pending, Completed, Cancelled, Failed, Timed_Out);

   -- ==========================================================================
   -- Cancellation Support
   -- ==========================================================================
   
   -- Global cancellation token for cooperative cancellation
   protected type Cancellation_Token is
      procedure Request_Cancellation;
      function Is_Cancelled return Boolean;
      procedure Reset;
   private
      Cancelled : Boolean := False;
   end Cancellation_Token;
   
   -- Global cancellation instance
   Global_Cancellation : Cancellation_Token;
   
   -- Check if current operation should continue
   function Should_Continue_Operation return Boolean;

   -- ==========================================================================
   -- Timeout Configuration
   -- ==========================================================================
   
   type Timeout_Config is record
      Default_Operation_Timeout : Duration := 30.0;
      Task_Startup_Timeout : Duration := 60.0;
      Task_Shutdown_Timeout : Duration := 5.0;
      Result_Retrieval_Timeout : Duration := 30.0;
   end record;
   
   -- Global timeout configuration
   Global_Timeout_Config : Timeout_Config;
   
   -- Timeout configuration procedures
   procedure Set_Default_Operation_Timeout (Timeout : Duration);
   procedure Set_Task_Startup_Timeout (Timeout : Duration);
   procedure Set_Task_Shutdown_Timeout (Timeout : Duration);
   procedure Set_Result_Retrieval_Timeout (Timeout : Duration);
   
   function Get_Default_Operation_Timeout return Duration;
   function Get_Task_Startup_Timeout return Duration;
   function Get_Task_Shutdown_Timeout return Duration;
   function Get_Result_Retrieval_Timeout return Duration;

   -- ==========================================================================
   -- Async Map Operations
   -- ==========================================================================
   
   generic
      type New_Value_Type is private;
      with procedure Transform (Input : Core.Value_Type; Output : out New_Value_Type);
      with function Copy_New_Value (Source : New_Value_Type) return New_Value_Type is <>;
      with function Default_New_Value return New_Value_Type is <>;
   package Async_Map_Operations is
      -- Result type for the transformed value
      type New_Result_Type is new Ada.Finalization.Controlled with private;
      
      -- Async operation handle
      type Async_Map_Result_Type is new Ada.Finalization.Limited_Controlled with private;
      
      -- Operations for New_Result_Type
      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Core.Error_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Core.Error_Type; Message : String);
      function Is_Ok (R : New_Result_Type) return Boolean;
      function Is_Err (R : New_Result_Type) return Boolean;
      function Unwrap (R : New_Result_Type) return New_Value_Type;
      function Unwrap_Err (R : New_Result_Type) return Core.Error_Type;
      
      -- Start async transformation
      procedure Start_Async_Map (R : Core.Result_Type; Async_R : out Async_Map_Result_Type);
      
      -- Check if async operation is complete
      function Is_Ready (Async_R : Async_Map_Result_Type) return Boolean;
      
      -- Get result status
      function Get_Status (Async_R : Async_Map_Result_Type) return Async_Status;
      
      -- Wait for completion and get result
      procedure Wait_For_Transformed_Result (Async_R : Async_Map_Result_Type; 
                                           New_R : out New_Result_Type);
      
      -- Wait for completion with timeout
      procedure Wait_For_Transformed_Result_With_Timeout (
         Async_R : Async_Map_Result_Type; 
         New_R : out New_Result_Type;
         Timeout : Duration;
         Timed_Out : out Boolean
      );
      
      -- Try to get result without waiting
      function Try_Get_Transformed_Result (Async_R : Async_Map_Result_Type; 
                                         New_R : out New_Result_Type) return Boolean;
      
      -- Cancel async operation
      procedure Cancel (Async_R : in out Async_Map_Result_Type);
      
      -- Get execution metrics
      function Get_Execution_Time (Async_R : Async_Map_Result_Type) return Duration;
      
   private
      -- New result type for transformed values
      type New_Result_Type is new Ada.Finalization.Controlled with record
         State : Core.Result_State := Core.Error;
         Value : New_Value_Type := Default_New_Value;
         Error : Core.Error_Type := Core.Default_Error;
         Message : Unbounded_String := Null_Unbounded_String;
         Is_Initialized : Boolean := False;
      end record;

      overriding procedure Initialize (Object : in out New_Result_Type);
      overriding procedure Adjust (Object : in out New_Result_Type);
      overriding procedure Finalize (Object : in out New_Result_Type);
      
      protected type Status_Manager is
         procedure Set_Status (New_Status : Async_Status);
         function Get_Status return Async_Status;
         procedure Cancel_If_Pending (Was_Cancelled : out Boolean);
         procedure Set_Start_Time (Time : Ada.Calendar.Time);
         procedure Set_End_Time (Time : Ada.Calendar.Time);
         function Get_Execution_Time return Duration;
         -- Synchronization for task startup
         entry Wait_Until_Started;
         procedure Signal_Started;
         -- Wait for completion
         entry Wait_For_Completion;
      private
         Current_Status : Async_Status := Pending;
         Start_Time : Ada.Calendar.Time;
         End_Time : Ada.Calendar.Time;
         Has_Start_Time : Boolean := False;
         Has_End_Time : Boolean := False;
         Task_Started : Boolean := False;
         Is_Complete : Boolean := False;
      end Status_Manager;
      
      task type Map_Task is
         entry Start (Input : Core.Result_Type);
         entry Get_Result (Output : out New_Result_Type);
         entry Get_Result_With_Timeout (Output : out New_Result_Type; 
                                       Timeout : Duration; 
                                       Success : out Boolean);
         entry Cancel;
         entry Shutdown; -- Proper task termination
      end Map_Task;
      
      type Async_Map_Result_Type is new Ada.Finalization.Limited_Controlled with record
         Manager : aliased Status_Manager;
         Task_Instance : Map_Task;
         Is_Started : Boolean := False;
      end record;
      
      overriding procedure Finalize (Object : in out Async_Map_Result_Type);
      
      -- RAII wrapper for automatic cleanup
      type Auto_Cleanup_Async_Result is new Ada.Finalization.Limited_Controlled with record
         Async_Result_Ptr : access Async_Map_Result_Type;
      end record;
      
      overriding procedure Finalize (Wrapper : in out Auto_Cleanup_Async_Result);
   end Async_Map_Operations;

   -- ==========================================================================
   -- Future-like Operations
   -- ==========================================================================
   
   -- Simple future type for async computations
   task type Async_Computation_Task is
      entry Start (Input : Core.Value_Type);
      entry Get_Result (R : out Core.Result_Type);
      entry Cancel;
      entry Shutdown;
   end Async_Computation_Task;

   type Future_Result_Type is tagged limited private;
   
   -- Create a future from a computation
   generic
      with procedure Compute (Input : Core.Value_Type; Output : out Core.Result_Type);
   package Future_Operations is
      procedure Start_Future (Input : Core.Value_Type; 
                            Future : out Future_Result_Type);
      
      function Is_Ready (Future : Future_Result_Type) return Boolean;
      
      procedure Wait_For_Result (Future : Future_Result_Type; 
                               R : out Core.Result_Type);
      
      procedure Cancel_Future (Future : in out Future_Result_Type);
   end Future_Operations;

   -- ==========================================================================
   -- Async Combinators
   -- ==========================================================================
   
   generic
      type Result_Array_Type is array (Positive range <>) of Core.Result_Type;
   package Async_Combinators is
      -- Wait for all operations to complete
      procedure Wait_All (
         Results : in out Result_Array_Type;
         Timeout : Duration := Duration'Last
      );
      
      -- Wait for any operation to complete  
      procedure Wait_Any (
         Results : in out Result_Array_Type;
         Index : out Positive;
         Result : out Core.Result_Type;
         Timeout : Duration := Duration'Last
      );
      
      -- Race multiple operations (first to complete wins)
      procedure Race (
         Results : in out Result_Array_Type;
         Winner_Index : out Positive;
         Result : out Core.Result_Type;
         Timeout : Duration := Duration'Last
      );
   end Async_Combinators;

   -- ==========================================================================
   -- Parallel Operations
   -- ==========================================================================
   
   generic
      type Result_Array_Type is array (Positive range <>) of Core.Result_Type;
      with procedure Process_Value (Input : Core.Value_Type; 
                                   Output : out Core.Value_Type);
   package Parallel_Operations is
      -- Process multiple Results concurrently
      procedure Process_All_Parallel (Results : in out Result_Array_Type);
      
      -- Process with configurable worker count
      procedure Process_All_Parallel (Results : in out Result_Array_Type; 
                                    Worker_Count : Positive);
   end Parallel_Operations;

   -- ==========================================================================
   -- Async Pipeline
   -- ==========================================================================
   
   generic
      type Intermediate_Type is private;
      type Final_Type is private;
      with procedure First_Transform (Input : Core.Value_Type; 
                                    Output : out Intermediate_Type);
      with procedure Second_Transform (Input : Intermediate_Type; 
                                     Output : out Final_Type);
      with function Copy_Final (Source : Final_Type) return Final_Type is <>;
      with function Default_Final return Final_Type is <>;
   package Async_Pipeline_Operations is
      type Pipeline_Result_Type is tagged limited private;
      
      -- Result type for final value
      type Final_Result_Type is new Ada.Finalization.Controlled with private;
      
      -- Operations for Final_Result_Type
      function Is_Ok (R : Final_Result_Type) return Boolean;
      function Is_Err (R : Final_Result_Type) return Boolean;
      function Unwrap (R : Final_Result_Type) return Final_Type;
      function Unwrap_Err (R : Final_Result_Type) return Core.Error_Type;
      
      -- Start async pipeline
      procedure Start_Pipeline (R : Core.Result_Type; 
                              Pipeline_R : out Pipeline_Result_Type);
      
      -- Wait for pipeline completion
      procedure Wait_For_Pipeline (Pipeline_R : Pipeline_Result_Type; 
                                 Final_R : out Final_Result_Type);
      
      -- Get pipeline status
      function Get_Pipeline_Status (Pipeline_R : Pipeline_Result_Type) 
                                  return Async_Status;
      
   private
      type Final_Result_Type is new Ada.Finalization.Controlled with record
         State : Core.Result_State := Core.Error;
         Value : Final_Type := Default_Final;
         Error : Core.Error_Type := Core.Default_Error;
         Message : Unbounded_String := Null_Unbounded_String;
         Is_Initialized : Boolean := False;
      end record;
      
      task type Pipeline_Task is
         entry Start (Input : Core.Result_Type);
         entry Get_Result (Output : out Final_Result_Type);
         entry Cancel;
      end Pipeline_Task;
      
      type Pipeline_Result_Type is tagged limited record
         Task_Instance : Pipeline_Task;
         Status : Async_Status := Pending;
      end record;
   end Async_Pipeline_Operations;

private
   -- Private implementation of Future_Result_Type
   type Future_Result_Type is tagged limited record
      Status : Async_Status := Pending;
      Result : Core.Result_Type;
      Task_Id : Ada.Task_Identification.Task_Id := 
                Ada.Task_Identification.Null_Task_Id;
   end record;

end Async_Result.Async;