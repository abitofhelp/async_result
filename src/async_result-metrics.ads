-- =============================================================================
-- Ada Async_Result Library - Metrics and Monitoring
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Performance monitoring, metrics collection, and diagnostics
-- Provides thread-safe counters, timers, and statistics
-- =============================================================================

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Async_Result.Metrics is
   pragma Elaborate_Body;

   use Ada.Strings.Unbounded;

   -- ==========================================================================
   -- Core Metric Types
   -- ==========================================================================
   
   -- Basic counter types
   type Counter_Value is new Natural;
   type Gauge_Value is new Integer;
   type Timer_Value is new Duration;
   
   -- Metric categories
   type Metric_Category is (
      Core_Operations,      -- Basic Result operations
      Async_Operations,     -- Async transformations
      Pool_Operations,      -- Thread pool metrics
      Error_Handling,       -- Error tracking
      Performance,          -- Performance metrics
      Resource_Usage        -- Memory and resource tracking
   );
   
   -- Metric severity for alerts
   type Metric_Severity is (Info, Warning, Error, Critical);

   -- ==========================================================================
   -- Atomic Counters
   -- ==========================================================================
   
   -- Thread-safe counter for incrementing values
   protected type Atomic_Counter is
      procedure Increment (New_Value : out Counter_Value);
      procedure Increment_By (Amount : Counter_Value; 
                            New_Value : out Counter_Value);
      procedure Decrement (New_Value : out Counter_Value);
      procedure Decrement_By (Amount : Counter_Value; 
                            New_Value : out Counter_Value);
      function Get_Value return Counter_Value;
      procedure Set_Value (Value : Counter_Value);
      procedure Reset;
   private
      Counter : Counter_Value := 0;
   end Atomic_Counter;
   
   -- Thread-safe gauge for values that go up and down
   protected type Atomic_Gauge is
      procedure Set (Value : Gauge_Value);
      procedure Increment (New_Value : out Gauge_Value);
      procedure Decrement (New_Value : out Gauge_Value);
      function Get_Value return Gauge_Value;
      procedure Reset;
   private
      Gauge : Gauge_Value := 0;
   end Atomic_Gauge;

   -- ==========================================================================
   -- Timing and Histograms
   -- ==========================================================================
   
   -- Timer for measuring durations
   type Timer_Handle is private;
   
   function Start_Timer return Timer_Handle;
   function Stop_Timer (Handle : Timer_Handle) return Duration;
   
   -- Histogram for tracking value distributions
   protected type Histogram is
      procedure Record_Value (Value : Duration);
      function Get_Count return Natural;
      function Get_Min return Duration;
      function Get_Max return Duration;
      function Get_Mean return Duration;
      function Get_Median return Duration;
      function Get_Percentile (P : Float) return Duration;
      procedure Reset;
   private
      type Duration_Array is array (Positive range <>) of Duration;
      type Duration_Array_Access is access Duration_Array;
      
      Values : Duration_Array_Access;
      Count : Natural := 0;
      Capacity : Natural := 1000;
      Sum : Duration := 0.0;
      Min_Value : Duration := Duration'Last;
      Max_Value : Duration := 0.0;
      
      procedure Resize_If_Needed;
   end Histogram;

   -- ==========================================================================
   -- Comprehensive Metrics
   -- ==========================================================================
   
   -- Async operation metrics
   type Async_Metrics_Type is record
      Tasks_Started : Counter_Value := 0;
      Tasks_Completed : Counter_Value := 0;
      Tasks_Failed : Counter_Value := 0;
      Tasks_Cancelled : Counter_Value := 0;
      Tasks_Timed_Out : Counter_Value := 0;
      Current_Active_Tasks : Gauge_Value := 0;
      Peak_Active_Tasks : Counter_Value := 0;
      Total_Execution_Time : Timer_Value := 0.0;
      Average_Execution_Time : Timer_Value := 0.0;
      Min_Execution_Time : Timer_Value := Timer_Value'Last;
      Max_Execution_Time : Timer_Value := 0.0;
   end record;
   
   -- Core operation metrics
   type Core_Metrics_Type is record
      Results_Created : Counter_Value := 0;
      Ok_Results : Counter_Value := 0;
      Error_Results : Counter_Value := 0;
      Unwrap_Operations : Counter_Value := 0;
      Unwrap_Failures : Counter_Value := 0;
      Map_Operations : Counter_Value := 0;
      And_Then_Operations : Counter_Value := 0;
      Pattern_Matches : Counter_Value := 0;
   end record;
   
   -- Error tracking metrics
   type Error_Metrics_Type is record
      Total_Errors : Counter_Value := 0;
      Errors_By_Category : array (1 .. 10) of Counter_Value := (others => 0);
      Last_Error_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
      Error_Rate_Per_Minute : Float := 0.0;
   end record;
   
   -- Resource usage metrics
   type Resource_Metrics_Type is record
      Memory_Allocated : Gauge_Value := 0;
      Memory_Deallocated : Gauge_Value := 0;
      Current_Memory_Usage : Gauge_Value := 0;
      Peak_Memory_Usage : Gauge_Value := 0;
      Active_Resources : Gauge_Value := 0;
   end record;

   -- ==========================================================================
   -- Global Metrics Manager
   -- ==========================================================================
   
   protected type Metrics_Manager is
      -- Async metrics
      procedure Record_Task_Start;
      procedure Record_Task_Complete (Execution_Time : Duration);
      procedure Record_Task_Failed;
      procedure Record_Task_Cancelled;
      procedure Record_Task_Timed_Out;
      function Get_Async_Metrics return Async_Metrics_Type;
      
      -- Core metrics
      procedure Record_Result_Created (Is_Ok : Boolean);
      procedure Record_Unwrap (Success : Boolean);
      procedure Record_Map_Operation;
      procedure Record_And_Then_Operation;
      procedure Record_Pattern_Match;
      function Get_Core_Metrics return Core_Metrics_Type;
      
      -- Error metrics
      procedure Record_Error (Category : Natural := 1);
      function Get_Error_Metrics return Error_Metrics_Type;
      
      -- Resource metrics
      procedure Record_Memory_Allocated (Bytes : Natural);
      procedure Record_Memory_Deallocated (Bytes : Natural);
      procedure Record_Resource_Created;
      procedure Record_Resource_Destroyed;
      function Get_Resource_Metrics return Resource_Metrics_Type;
      
      -- General operations
      procedure Reset_All_Metrics;
      procedure Reset_Category (Category : Metric_Category);
      
   private
      -- Metric storage
      Async_Stats : Async_Metrics_Type;
      Core_Stats : Core_Metrics_Type;
      Error_Stats : Error_Metrics_Type;
      Resource_Stats : Resource_Metrics_Type;
      
      -- Internal counters
      Task_Count_For_Average : Natural := 0;
      Current_Active_Tasks : Natural := 0;
      
      -- Helper procedures
      procedure Update_Task_Average (New_Time : Duration);
      procedure Update_Error_Rate;
   end Metrics_Manager;
   
   -- Global metrics instance
   Global_Metrics : Metrics_Manager;

   -- ==========================================================================
   -- Metric Reporting
   -- ==========================================================================
   
   -- Metric snapshot for reporting
   type Metric_Snapshot is record
      Timestamp : Ada.Calendar.Time;
      Category : Metric_Category;
      Name : Unbounded_String;
      Value : Float;
      Unit : Unbounded_String;
      Tags : Unbounded_String;
   end record;
   
   -- Package for collecting metric snapshots
   package Metric_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Metric_Snapshot
   );
   
   -- Get all metrics as snapshots
   function Get_All_Metrics return Metric_Vectors.Vector;
   
   -- Format metrics for display
   function Format_Metrics (Format : String := "text") return String;
   
   -- Export metrics in various formats
   procedure Export_Metrics_CSV (Filename : String);
   procedure Export_Metrics_JSON (Filename : String);
   procedure Export_Metrics_Prometheus (Filename : String);

   -- ==========================================================================
   -- Performance Profiling
   -- ==========================================================================
   
   -- Profiler for detailed performance analysis
   type Profiler is tagged limited private;
   
   procedure Start_Profile (P : in out Profiler; Name : String);
   procedure End_Profile (P : in out Profiler);
   procedure Add_Checkpoint (P : in out Profiler; Name : String);
   function Get_Profile_Report (P : Profiler) return String;
   
   -- Scoped profiler using RAII
   type Scoped_Profiler (Name_Length : Natural) is 
      new Ada.Finalization.Limited_Controlled with record
      Name : String (1 .. Name_Length);
      Start_Time : Ada.Calendar.Time;
   end record;
   
   overriding procedure Initialize (SP : in out Scoped_Profiler);
   overriding procedure Finalize (SP : in out Scoped_Profiler);

   -- ==========================================================================
   -- Alerts and Thresholds
   -- ==========================================================================
   
   -- Alert configuration
   type Alert_Config is record
      Name : Unbounded_String;
      Category : Metric_Category;
      Threshold : Float;
      Comparison : Character; -- '>', '<', '='
      Severity : Metric_Severity;
      Enabled : Boolean := True;
   end record;
   
   -- Alert manager
   protected type Alert_Manager is
      procedure Add_Alert (Config : Alert_Config);
      procedure Remove_Alert (Name : String);
      procedure Check_Alerts;
      function Get_Active_Alerts return Natural;
      entry Wait_For_Alert (Alert : out Alert_Config);
   private
      type Alert_Array is array (1 .. 100) of Alert_Config;
      Alerts : Alert_Array;
      Alert_Count : Natural := 0;
      Active_Alerts : Natural := 0;
      Has_Alert : Boolean := False;
   end Alert_Manager;
   
   -- Global alert manager
   Global_Alerts : Alert_Manager;

   -- ==========================================================================
   -- Diagnostics
   -- ==========================================================================
   
   -- System diagnostics
   function Get_System_Info return String;
   function Get_Memory_Info return String;
   function Get_Task_Info return String;
   
   -- Health check
   type Health_Status is (Healthy, Degraded, Unhealthy);
   
   function Check_Health return Health_Status;
   function Get_Health_Report return String;

private
   -- Timer implementation
   type Timer_Handle is record
      Start_Time : Ada.Calendar.Time;
   end record;
   
   -- Profiler implementation
   type Profile_Entry is record
      Name : Unbounded_String;
      Start_Time : Ada.Calendar.Time;
      End_Time : Ada.Calendar.Time;
      Duration : Duration;
   end record;
   
   package Profile_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Profile_Entry
   );
   
   type Profiler is tagged limited record
      Entries : Profile_Vectors.Vector;
      Current_Entry : Profile_Entry;
      Is_Active : Boolean := False;
   end record;

end Async_Result.Metrics;