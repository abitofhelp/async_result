-- =============================================================================
-- Ada Async_Result Library - Work-Stealing Pool
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- High-performance work-stealing thread pool for concurrent task execution
-- Provides dynamic scaling, backpressure control, and metrics
-- =============================================================================

with Ada.Finalization;
with Ada.Strings.Unbounded;

generic
   -- Import the core types from parent instantiation
   with package Core is new Async_Result.Core (<>);
   -- Work item type to be processed
   type Work_Item_Type is private;
   -- Processing function that transforms work items into results
   with procedure Process_Work (Item : Work_Item_Type; 
                               Result : out Core.Result_Type);
package Async_Result.Pool is
   pragma Elaborate_Body;

   use Ada.Strings.Unbounded;

   -- ==========================================================================
   -- Pool Configuration
   -- ==========================================================================
   
   -- Backpressure policies for queue management
   type Backpressure_Policy is (
      Block,        -- Block when queue is full
      Drop_Oldest,  -- Drop oldest items when queue is full
      Drop_Newest,  -- Drop newest items when queue is full  
      Fail_Fast     -- Immediately fail when queue is full
   );
   
   -- Dynamic queue sizing configuration
   type Dynamic_Queue_Config is record
      Enable_Dynamic_Sizing : Boolean := True;
      Min_Queue_Size : Natural := 100;
      Max_Queue_Size : Natural := 10_000;
      Resize_Threshold : Float := 0.8;  -- Resize when 80% full
      Resize_Factor : Float := 2.0;     -- Double size when resizing
   end record;
   
   -- Worker configuration
   type Worker_Config is record
      Stack_Size : Natural := 0;  -- 0 means use default
      Priority : Natural := 0;    -- 0 means use default
   end record;

   -- ==========================================================================
   -- Pool Metrics
   -- ==========================================================================
   
   type Pool_Metrics_Type is record
      Tasks_Submitted : Natural := 0;
      Tasks_Completed : Natural := 0;
      Tasks_Failed : Natural := 0;
      Tasks_Rejected : Natural := 0;
      Active_Workers : Natural := 0;
      Idle_Workers : Natural := 0;
      Queue_Length : Natural := 0;
      Queue_Capacity : Natural := 0;
      Total_Processing_Time : Duration := 0.0;
      Average_Processing_Time : Duration := 0.0;
      Min_Processing_Time : Duration := Duration'Last;
      Max_Processing_Time : Duration := 0.0;
   end record;

   -- ==========================================================================
   -- Main Pool Type
   -- ==========================================================================
   
   type Task_Pool_Type is new Ada.Finalization.Limited_Controlled with private;
   
   -- Pool initialization and configuration
   procedure Initialize_Pool (
      Pool : in out Task_Pool_Type; 
      Worker_Count : Positive;
      Initial_Queue_Size : Natural := 1000;
      Backpressure : Backpressure_Policy := Block;
      Dynamic_Config : Dynamic_Queue_Config := (others => <>);
      Worker_Configuration : Worker_Config := (others => <>)
   );
   
   -- Submit work to pool
   procedure Submit_Work (
      Pool : in out Task_Pool_Type; 
      Item : Work_Item_Type
   );
   
   -- Try to submit work (non-blocking)
   function Try_Submit_Work (
      Pool : in out Task_Pool_Type; 
      Item : Work_Item_Type
   ) return Boolean;
   
   -- Submit work with callback for result
   generic
      with procedure On_Complete (Result : Core.Result_Type);
   procedure Submit_Work_With_Callback (
      Pool : in out Task_Pool_Type;
      Item : Work_Item_Type
   );
   
   -- Batch submit multiple work items
   type Work_Item_Array is array (Positive range <>) of Work_Item_Type;
   
   procedure Submit_Batch (
      Pool : in out Task_Pool_Type;
      Items : Work_Item_Array
   );
   
   -- Get pool metrics
   function Get_Metrics (Pool : Task_Pool_Type) return Pool_Metrics_Type;
   
   -- Pool control operations
   procedure Pause_Pool (Pool : in out Task_Pool_Type);
   procedure Resume_Pool (Pool : in out Task_Pool_Type);
   procedure Shutdown_Pool (Pool : in out Task_Pool_Type);
   procedure Shutdown_Pool_Gracefully (
      Pool : in out Task_Pool_Type;
      Timeout : Duration := 30.0
   );
   
   -- Dynamic worker management
   procedure Add_Workers (
      Pool : in out Task_Pool_Type;
      Count : Positive
   );
   
   procedure Remove_Workers (
      Pool : in out Task_Pool_Type;
      Count : Positive
   );
   
   procedure Set_Worker_Count (
      Pool : in out Task_Pool_Type;
      New_Count : Positive
   );
   
   -- Queue management
   function Get_Queue_Length (Pool : Task_Pool_Type) return Natural;
   function Get_Queue_Capacity (Pool : Task_Pool_Type) return Natural;
   procedure Clear_Queue (Pool : in out Task_Pool_Type);
   
   -- Backpressure control
   procedure Set_Backpressure_Policy (
      Pool : in out Task_Pool_Type;
      Policy : Backpressure_Policy
   );
   
   function Get_Backpressure_Policy (Pool : Task_Pool_Type) 
                                   return Backpressure_Policy;

   -- ==========================================================================
   -- Advanced Features
   -- ==========================================================================
   
   -- Priority work items
   generic
      type Priority_Type is (<>);
   package Priority_Pool is
      type Priority_Work_Item is record
         Item : Work_Item_Type;
         Priority : Priority_Type;
      end record;
      
      procedure Submit_Priority_Work (
         Pool : in out Task_Pool_Type;
         Item : Work_Item_Type;
         Priority : Priority_Type
      );
   end Priority_Pool;
   
   -- Work stealing between pools
   procedure Enable_Work_Stealing (
      Source_Pool : in out Task_Pool_Type;
      Target_Pool : in out Task_Pool_Type
   );
   
   procedure Disable_Work_Stealing (
      Source_Pool : in out Task_Pool_Type;
      Target_Pool : in out Task_Pool_Type
   );

private
   -- ==========================================================================
   -- Internal Types
   -- ==========================================================================
   
   -- Forward declaration for access type
   type Work_Queue;
   type Work_Queue_Access is access all Work_Queue;
   
   -- Queue implementation with dynamic sizing
   type Work_Item_Array_Access is access Work_Item_Array;
   
   protected type Work_Queue is
      procedure Add_Work (Item : Work_Item_Type);
      procedure Try_Add_Work (Item : Work_Item_Type; Success : out Boolean);
      entry Get_Work (Item : out Work_Item_Type);
      procedure Shutdown;
      function Get_Queue_Length return Natural;
      function Get_Queue_Capacity return Natural;
      procedure Set_Backpressure_Policy (Policy : Backpressure_Policy);
      procedure Clear;
      procedure Resize_If_Needed;
   private
      Queue : Work_Item_Array_Access;
      Head : Natural := 1;
      Tail : Natural := 1;
      Count : Natural := 0;
      Is_Shutdown : Boolean := False;
      Current_Capacity : Natural := 1000;
      Dynamic_Config : Dynamic_Queue_Config;
      Backpressure : Backpressure_Policy := Block;
      
      procedure Resize_Queue (New_Size : Natural);
   end Work_Queue;
   
   -- Metrics manager
   protected type Pool_Metrics_Manager is
      procedure Record_Task_Submitted;
      procedure Record_Task_Completed (Processing_Time : Duration);
      procedure Record_Task_Failed;
      procedure Record_Task_Rejected;
      procedure Update_Worker_Count (Active, Idle : Natural);
      procedure Update_Queue_Stats (Length, Capacity : Natural);
      function Get_Metrics return Pool_Metrics_Type;
      procedure Reset_Metrics;
   private
      Metrics : Pool_Metrics_Type;
      Task_Count_For_Average : Natural := 0;
   end Pool_Metrics_Manager;
   
   -- Backpressure controller
   protected type Backpressure_Controller is
      procedure Configure (
         Policy : Backpressure_Policy; 
         Max_Queue_Size : Natural
      );
      function Should_Accept_Work (Current_Queue_Size : Natural) return Boolean;
      procedure Work_Submitted;
      procedure Work_Completed;
      function Get_Policy return Backpressure_Policy;
   private
      Current_Policy : Backpressure_Policy := Block;
      Max_Queue : Natural := 1000;
      Current_Load : Natural := 0;
   end Backpressure_Controller;
   
   -- Worker task type
   task type Worker_Task (
      Queue_Access : Work_Queue_Access;
      Metrics_Access : access Pool_Metrics_Manager;
      Worker_ID : Natural
   ) is
      entry Start;
      entry Shutdown;
   end Worker_Task;
   
   type Worker_Task_Access is access Worker_Task;
   type Worker_Array is array (Positive range <>) of Worker_Task_Access;
   type Worker_Array_Access is access Worker_Array;
   
   -- Pool state management
   protected type Pool_State_Manager is
      procedure Set_Paused (Value : Boolean);
      function Is_Paused return Boolean;
      procedure Set_Shutdown (Value : Boolean);
      function Is_Shutdown return Boolean;
   private
      Paused : Boolean := False;
      Shutdown : Boolean := False;
   end Pool_State_Manager;
   
   -- Main pool implementation
   type Task_Pool_Type is new Ada.Finalization.Limited_Controlled with record
      Queue : aliased Work_Queue;
      Workers : Worker_Array_Access;
      Worker_Count : Natural := 0;
      Max_Workers : Natural := 100;
      Metrics : aliased Pool_Metrics_Manager;
      Backpressure : aliased Backpressure_Controller;
      State : aliased Pool_State_Manager;
      Is_Initialized : Boolean := False;
   end record;
   
   -- RAII cleanup
   overriding procedure Finalize (Pool : in out Task_Pool_Type);

end Async_Result.Pool;