-- =============================================================================
-- Ada Async_Result Library - Metrics and Monitoring Body
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Implementation of performance monitoring and metrics collection
-- =============================================================================

with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Async_Result.Metrics is

   ----------------------------------------------------------------------------
   -- Atomic Counter Implementation
   ----------------------------------------------------------------------------

   protected body Atomic_Counter is
      
      procedure Increment (New_Value : out Counter_Value) is
      begin
         Counter := Counter + 1;
         New_Value := Counter;
      end Increment;

      procedure Increment_By (Amount : Counter_Value; New_Value : out Counter_Value) is
      begin
         Counter := Counter + Amount;
         New_Value := Counter;
      end Increment_By;

      procedure Decrement (New_Value : out Counter_Value) is
      begin
         if Counter > 0 then
            Counter := Counter - 1;
         end if;
         New_Value := Counter;
      end Decrement;

      procedure Decrement_By (Amount : Counter_Value; New_Value : out Counter_Value) is
      begin
         if Counter >= Amount then
            Counter := Counter - Amount;
         else
            Counter := 0;
         end if;
         New_Value := Counter;
      end Decrement_By;

      function Get_Value return Counter_Value is
      begin
         return Counter;
      end Get_Value;

      procedure Set_Value (Value : Counter_Value) is
      begin
         Counter := Value;
      end Set_Value;

      procedure Reset is
      begin
         Counter := 0;
      end Reset;

   end Atomic_Counter;

   ----------------------------------------------------------------------------
   -- Atomic Gauge Implementation
   ----------------------------------------------------------------------------

   protected body Atomic_Gauge is
      
      procedure Set (Value : Gauge_Value) is
      begin
         Gauge := Value;
      end Set;

      procedure Increment (New_Value : out Gauge_Value) is
      begin
         Gauge := Gauge + 1;
         New_Value := Gauge;
      end Increment;

      procedure Decrement (New_Value : out Gauge_Value) is
      begin
         Gauge := Gauge - 1;
         New_Value := Gauge;
      end Decrement;

      function Get_Value return Gauge_Value is
      begin
         return Gauge;
      end Get_Value;

      procedure Reset is
      begin
         Gauge := 0;
      end Reset;

   end Atomic_Gauge;

   ----------------------------------------------------------------------------
   -- Timer Implementation
   ----------------------------------------------------------------------------

   function Start_Timer return Timer_Handle is
   begin
      return (Start_Time => Ada.Calendar.Clock);
   end Start_Timer;

   function Stop_Timer (Handle : Timer_Handle) return Duration is
   begin
      return Ada.Calendar.Clock - Handle.Start_Time;
   end Stop_Timer;

   ----------------------------------------------------------------------------
   -- Histogram Implementation
   ----------------------------------------------------------------------------

   protected body Histogram is
      
      procedure Record_Value (Value : Duration) is
      begin
         Resize_If_Needed;
         
         Count := Count + 1;
         if Values /= null and then Count <= Values'Length then
            Values (Count) := Value;
         end if;
         
         Sum := Sum + Value;
         
         if Value < Min_Value then
            Min_Value := Value;
         end if;
         
         if Value > Max_Value then
            Max_Value := Value;
         end if;
      end Record_Value;

      function Get_Count return Natural is
      begin
         return Count;
      end Get_Count;

      function Get_Min return Duration is
      begin
         if Count = 0 then
            return 0.0;
         else
            return Min_Value;
         end if;
      end Get_Min;

      function Get_Max return Duration is
      begin
         return Max_Value;
      end Get_Max;

      function Get_Mean return Duration is
      begin
         if Count = 0 then
            return 0.0;
         else
            return Sum / Duration (Count);
         end if;
      end Get_Mean;

      function Get_Median return Duration is
      begin
         -- Simplified implementation - would need sorting for accurate median
         if Count = 0 then
            return 0.0;
         else
            return Get_Mean;
         end if;
      end Get_Median;

      function Get_Percentile (P : Float) return Duration is
      begin
         -- Simplified implementation
         if Count = 0 then
            return 0.0;
         elsif P <= 0.0 then
            return Get_Min;
         elsif P >= 100.0 then
            return Get_Max;
         else
            return Get_Mean;
         end if;
      end Get_Percentile;

      procedure Reset is
      begin
         Count := 0;
         Sum := 0.0;
         Min_Value := Duration'Last;
         Max_Value := 0.0;
      end Reset;

      procedure Resize_If_Needed is
         procedure Free is new Ada.Unchecked_Deallocation (Duration_Array, Duration_Array_Access);
         New_Capacity : Natural;
         New_Values : Duration_Array_Access;
      begin
         if Values = null then
            Values := new Duration_Array (1 .. Capacity);
         elsif Count >= Values'Length then
            New_Capacity := Capacity * 2;
            New_Values := new Duration_Array (1 .. New_Capacity);
            New_Values (1 .. Values'Length) := Values.all;
            Free (Values);
            Values := New_Values;
            Capacity := New_Capacity;
         end if;
      end Resize_If_Needed;

   end Histogram;

   ----------------------------------------------------------------------------
   -- Metrics Manager Implementation
   ----------------------------------------------------------------------------

   protected body Metrics_Manager is
      
      procedure Record_Task_Start is
         Dummy : Natural;
      begin
         Async_Stats.Tasks_Started := Async_Stats.Tasks_Started + 1;
         Current_Active_Tasks := Current_Active_Tasks + 1;
         Async_Stats.Current_Active_Tasks := Gauge_Value (Current_Active_Tasks);
         
         if Counter_Value (Current_Active_Tasks) > Async_Stats.Peak_Active_Tasks then
            Async_Stats.Peak_Active_Tasks := Counter_Value (Current_Active_Tasks);
         end if;
      end Record_Task_Start;

      procedure Record_Task_Complete (Execution_Time : Duration) is
      begin
         Async_Stats.Tasks_Completed := Async_Stats.Tasks_Completed + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
         Async_Stats.Current_Active_Tasks := Gauge_Value (Current_Active_Tasks);
         
         Update_Task_Average (Execution_Time);
         
         if Execution_Time < Async_Stats.Min_Execution_Time then
            Async_Stats.Min_Execution_Time := Timer_Value (Execution_Time);
         end if;
         
         if Execution_Time > Async_Stats.Max_Execution_Time then
            Async_Stats.Max_Execution_Time := Timer_Value (Execution_Time);
         end if;
      end Record_Task_Complete;

      procedure Record_Task_Failed is
      begin
         Async_Stats.Tasks_Failed := Async_Stats.Tasks_Failed + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
         Async_Stats.Current_Active_Tasks := Gauge_Value (Current_Active_Tasks);
      end Record_Task_Failed;

      procedure Record_Task_Cancelled is
      begin
         Async_Stats.Tasks_Cancelled := Async_Stats.Tasks_Cancelled + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
         Async_Stats.Current_Active_Tasks := Gauge_Value (Current_Active_Tasks);
      end Record_Task_Cancelled;

      procedure Record_Task_Timed_Out is
      begin
         Async_Stats.Tasks_Timed_Out := Async_Stats.Tasks_Timed_Out + 1;
         if Current_Active_Tasks > 0 then
            Current_Active_Tasks := Current_Active_Tasks - 1;
         end if;
         Async_Stats.Current_Active_Tasks := Gauge_Value (Current_Active_Tasks);
      end Record_Task_Timed_Out;

      function Get_Async_Metrics return Async_Metrics_Type is
      begin
         return Async_Stats;
      end Get_Async_Metrics;

      procedure Record_Result_Created (Is_Ok : Boolean) is
      begin
         Core_Stats.Results_Created := Core_Stats.Results_Created + 1;
         if Is_Ok then
            Core_Stats.Ok_Results := Core_Stats.Ok_Results + 1;
         else
            Core_Stats.Error_Results := Core_Stats.Error_Results + 1;
         end if;
      end Record_Result_Created;

      procedure Record_Unwrap (Success : Boolean) is
      begin
         Core_Stats.Unwrap_Operations := Core_Stats.Unwrap_Operations + 1;
         if not Success then
            Core_Stats.Unwrap_Failures := Core_Stats.Unwrap_Failures + 1;
         end if;
      end Record_Unwrap;

      procedure Record_Map_Operation is
      begin
         Core_Stats.Map_Operations := Core_Stats.Map_Operations + 1;
      end Record_Map_Operation;

      procedure Record_And_Then_Operation is
      begin
         Core_Stats.And_Then_Operations := Core_Stats.And_Then_Operations + 1;
      end Record_And_Then_Operation;

      procedure Record_Pattern_Match is
      begin
         Core_Stats.Pattern_Matches := Core_Stats.Pattern_Matches + 1;
      end Record_Pattern_Match;

      function Get_Core_Metrics return Core_Metrics_Type is
      begin
         return Core_Stats;
      end Get_Core_Metrics;

      procedure Record_Error (Category : Natural := 1) is
      begin
         Error_Stats.Total_Errors := Error_Stats.Total_Errors + 1;
         if Category in Error_Stats.Errors_By_Category'Range then
            Error_Stats.Errors_By_Category (Category) := 
               Error_Stats.Errors_By_Category (Category) + 1;
         end if;
         Error_Stats.Last_Error_Time := Ada.Calendar.Clock;
         Update_Error_Rate;
      end Record_Error;

      function Get_Error_Metrics return Error_Metrics_Type is
      begin
         return Error_Stats;
      end Get_Error_Metrics;

      procedure Record_Memory_Allocated (Bytes : Natural) is
      begin
         Resource_Stats.Memory_Allocated := Resource_Stats.Memory_Allocated + Gauge_Value (Bytes);
         Resource_Stats.Current_Memory_Usage := Resource_Stats.Current_Memory_Usage + Gauge_Value (Bytes);
         
         if Resource_Stats.Current_Memory_Usage > Resource_Stats.Peak_Memory_Usage then
            Resource_Stats.Peak_Memory_Usage := Resource_Stats.Current_Memory_Usage;
         end if;
      end Record_Memory_Allocated;

      procedure Record_Memory_Deallocated (Bytes : Natural) is
      begin
         Resource_Stats.Memory_Deallocated := Resource_Stats.Memory_Deallocated + Gauge_Value (Bytes);
         if Resource_Stats.Current_Memory_Usage >= Gauge_Value (Bytes) then
            Resource_Stats.Current_Memory_Usage := Resource_Stats.Current_Memory_Usage - Gauge_Value (Bytes);
         else
            Resource_Stats.Current_Memory_Usage := 0;
         end if;
      end Record_Memory_Deallocated;

      procedure Record_Resource_Created is
      begin
         Resource_Stats.Active_Resources := Resource_Stats.Active_Resources + 1;
      end Record_Resource_Created;

      procedure Record_Resource_Destroyed is
      begin
         if Resource_Stats.Active_Resources > 0 then
            Resource_Stats.Active_Resources := Resource_Stats.Active_Resources - 1;
         end if;
      end Record_Resource_Destroyed;

      function Get_Resource_Metrics return Resource_Metrics_Type is
      begin
         return Resource_Stats;
      end Get_Resource_Metrics;

      procedure Reset_All_Metrics is
      begin
         Async_Stats := (others => <>);
         Core_Stats := (others => <>);
         Error_Stats := (others => <>);
         Resource_Stats := (others => <>);
         Task_Count_For_Average := 0;
         Current_Active_Tasks := 0;
      end Reset_All_Metrics;

      procedure Reset_Category (Category : Metric_Category) is
      begin
         case Category is
            when Async_Operations =>
               Async_Stats := (others => <>);
               Current_Active_Tasks := 0;
            when Core_Operations =>
               Core_Stats := (others => <>);
            when Error_Handling =>
               Error_Stats := (others => <>);
            when Resource_Usage =>
               Resource_Stats := (others => <>);
            when others =>
               null;
         end case;
      end Reset_Category;

      procedure Update_Task_Average (New_Time : Duration) is
      begin
         Task_Count_For_Average := Task_Count_For_Average + 1;
         Async_Stats.Total_Execution_Time := Timer_Value (Duration (Async_Stats.Total_Execution_Time) + New_Time);
         Async_Stats.Average_Execution_Time := Timer_Value (Duration (Async_Stats.Total_Execution_Time) / Duration (Task_Count_For_Average));
      end Update_Task_Average;

      procedure Update_Error_Rate is
         -- Simplified error rate calculation
      begin
         if Natural (Error_Stats.Total_Errors) > 0 then
            Error_Stats.Error_Rate_Per_Minute := 
               Float (Error_Stats.Total_Errors) / 60.0; -- Simplified
         else
            Error_Stats.Error_Rate_Per_Minute := 0.0;
         end if;
      end Update_Error_Rate;

   end Metrics_Manager;

   ----------------------------------------------------------------------------
   -- Alert Manager Implementation
   ----------------------------------------------------------------------------

   protected body Alert_Manager is
      
      procedure Add_Alert (Config : Alert_Config) is
      begin
         if Alert_Count < Alerts'Length then
            Alert_Count := Alert_Count + 1;
            Alerts (Alert_Count) := Config;
         end if;
      end Add_Alert;

      procedure Remove_Alert (Name : String) is
         Found_Index : Natural := 0;
      begin
         for I in 1 .. Alert_Count loop
            if To_String (Alerts (I).Name) = Name then
               Found_Index := I;
               exit;
            end if;
         end loop;
         
         if Found_Index > 0 then
            -- Shift remaining alerts
            for I in Found_Index .. Alert_Count - 1 loop
               Alerts (I) := Alerts (I + 1);
            end loop;
            Alert_Count := Alert_Count - 1;
         end if;
      end Remove_Alert;

      procedure Check_Alerts is
      begin
         -- Implementation would check metrics against thresholds
         -- and set Has_Alert if any alerts are triggered
         null;
      end Check_Alerts;

      function Get_Active_Alerts return Natural is
      begin
         return Active_Alerts;
      end Get_Active_Alerts;

      entry Wait_For_Alert (Alert : out Alert_Config) when Has_Alert is
      begin
         -- Return the first active alert
         Alert := Alerts (1); -- Simplified
         Has_Alert := False;
      end Wait_For_Alert;

   end Alert_Manager;

   ----------------------------------------------------------------------------
   -- Metric Reporting
   ----------------------------------------------------------------------------

   function Get_All_Metrics return Metric_Vectors.Vector is
      Result : Metric_Vectors.Vector;
      Snapshot : Metric_Snapshot;
   begin
      -- Add async metrics
      Snapshot := (
         Timestamp => Ada.Calendar.Clock,
         Category => Async_Operations,
         Name => To_Unbounded_String ("tasks_started"),
         Value => Float (Global_Metrics.Get_Async_Metrics.Tasks_Started),
         Unit => To_Unbounded_String ("count"),
         Tags => Null_Unbounded_String
      );
      Result.Append (Snapshot);
      
      -- Add more metrics...
      
      return Result;
   end Get_All_Metrics;

   function Format_Metrics (Format : String := "text") return String is
   begin
      if Format = "json" then
         return "{""metrics"": []}"; -- Simplified
      else
         return "Metrics Report" & ASCII.LF &
                "=============" & ASCII.LF &
                "Tasks Started: " & Counter_Value'Image (Global_Metrics.Get_Async_Metrics.Tasks_Started);
      end if;
   end Format_Metrics;

   procedure Export_Metrics_CSV (Filename : String) is
   begin
      -- Implementation would write metrics to CSV file
      null;
   end Export_Metrics_CSV;

   procedure Export_Metrics_JSON (Filename : String) is
   begin
      -- Implementation would write metrics to JSON file
      null;
   end Export_Metrics_JSON;

   procedure Export_Metrics_Prometheus (Filename : String) is
   begin
      -- Implementation would write metrics in Prometheus format
      null;
   end Export_Metrics_Prometheus;

   ----------------------------------------------------------------------------
   -- Profiler Implementation
   ----------------------------------------------------------------------------

   procedure Start_Profile (P : in out Profiler; Name : String) is
   begin
      P.Current_Entry.Name := To_Unbounded_String (Name);
      P.Current_Entry.Start_Time := Ada.Calendar.Clock;
      P.Is_Active := True;
   end Start_Profile;

   procedure End_Profile (P : in out Profiler) is
   begin
      if P.Is_Active then
         P.Current_Entry.End_Time := Ada.Calendar.Clock;
         P.Current_Entry.Duration := P.Current_Entry.End_Time - P.Current_Entry.Start_Time;
         P.Entries.Append (P.Current_Entry);
         P.Is_Active := False;
      end if;
   end End_Profile;

   procedure Add_Checkpoint (P : in out Profiler; Name : String) is
      Entry_Rec : Profile_Entry;
   begin
      if P.Is_Active then
         Entry_Rec.Name := To_Unbounded_String (Name);
         Entry_Rec.Start_Time := P.Current_Entry.Start_Time;
         Entry_Rec.End_Time := Ada.Calendar.Clock;
         Entry_Rec.Duration := Entry_Rec.End_Time - Entry_Rec.Start_Time;
         P.Entries.Append (Entry_Rec);
      end if;
   end Add_Checkpoint;

   function Get_Profile_Report (P : Profiler) return String is
      Report : Unbounded_String := To_Unbounded_String ("Profile Report:" & ASCII.LF);
   begin
      for Entry_Rec of P.Entries loop
         Append (Report, To_String (Entry_Rec.Name) & ": " & 
                Duration'Image (Entry_Rec.Duration) & "s" & ASCII.LF);
      end loop;
      return To_String (Report);
   end Get_Profile_Report;

   ----------------------------------------------------------------------------
   -- Scoped Profiler
   ----------------------------------------------------------------------------

   overriding procedure Initialize (SP : in out Scoped_Profiler) is
   begin
      SP.Start_Time := Ada.Calendar.Clock;
   end Initialize;

   overriding procedure Finalize (SP : in out Scoped_Profiler) is
      Duration : constant Ada.Calendar.Time := Ada.Calendar.Clock - SP.Start_Time;
   begin
      -- Log or record the duration
      null;
   end Finalize;

   ----------------------------------------------------------------------------
   -- Diagnostics
   ----------------------------------------------------------------------------

   function Get_System_Info return String is
   begin
      return "System: Ada Runtime" & ASCII.LF &
             "Compiler: GNAT" & ASCII.LF;
   end Get_System_Info;

   function Get_Memory_Info return String is
   begin
      return "Memory Usage: " & Gauge_Value'Image (Global_Metrics.Get_Resource_Metrics.Current_Memory_Usage) & " bytes";
   end Get_Memory_Info;

   function Get_Task_Info return String is
   begin
      return "Active Tasks: " & Gauge_Value'Image (Global_Metrics.Get_Async_Metrics.Current_Active_Tasks);
   end Get_Task_Info;

   function Check_Health return Health_Status is
      Async_Stats : constant Async_Metrics_Type := Global_Metrics.Get_Async_Metrics;
   begin
      if Natural (Async_Stats.Tasks_Failed) > Natural (Async_Stats.Tasks_Completed) / 2 then
         return Unhealthy;
      elsif Natural (Async_Stats.Tasks_Failed) > Natural (Async_Stats.Tasks_Completed) / 10 then
         return Degraded;
      else
         return Healthy;
      end if;
   end Check_Health;

   function Get_Health_Report return String is
      Status : constant Health_Status := Check_Health;
   begin
      case Status is
         when Healthy =>
            return "System Status: Healthy";
         when Degraded =>
            return "System Status: Degraded - High failure rate";
         when Unhealthy =>
            return "System Status: Unhealthy - Critical failure rate";
      end case;
   end Get_Health_Report;

end Async_Result.Metrics;