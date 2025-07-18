-- =============================================================================
-- Ada Result Library - Async Performance Benchmarks
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Performance benchmarking suite for async operations measuring throughput,
-- latency, and scalability under various concurrent workloads.
-- =============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Float_Random;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Async_Result;

procedure Async_Performance_Benchmark is

   -- Benchmark configuration
   type Benchmark_Config is record
      Name : Unbounded_String;
      Iterations : Natural;
      Concurrent_Tasks : Natural;
      Warmup_Iterations : Natural;
   end record;

   -- Performance metrics
   type Performance_Metrics is record
      Total_Operations : Natural := 0;
      Total_Duration : Duration := 0.0;
      Min_Latency : Duration := Duration'Last;
      Max_Latency : Duration := 0.0;
      Avg_Latency : Duration := 0.0;
      Throughput : Float := 0.0; -- Operations per second
      Success_Rate : Float := 0.0;
   end record;

   -- Random number generation
   Float_Gen : Ada.Numerics.Float_Random.Generator;

   -- Test data types
   type Benchmark_Error is (No_Error, Timeout, Processing_Error);
   
   function Copy_Integer (Source : Integer) return Integer is (Source);
   function Copy_Error (Source : Benchmark_Error) return Benchmark_Error is (Source);
   function Copy_String (Source : Unbounded_String) return Unbounded_String is (Source);
   function Default_Integer return Integer is (0);
   function Default_Error return Benchmark_Error is (No_Error);
   function Default_String return Unbounded_String is (Null_Unbounded_String);
   
   package Integer_Result is new Async_Result (
      Value_Type => Integer,
      Error_Type => Benchmark_Error,
      Copy_Value => Copy_Integer,
      Copy_Error => Copy_Error,
      Default_Value => Default_Integer,
      Default_Error => Default_Error
   );
   use Integer_Result;

   package String_Result is new Async_Result (
      Value_Type => Unbounded_String,
      Error_Type => Benchmark_Error,
      Copy_Value => Copy_String,
      Copy_Error => Copy_Error,
      Default_Value => Default_String,
      Default_Error => Default_Error
   );

   -- Latency measurements storage
   package Duration_Vectors is new Ada.Containers.Vectors (
      Index_Type => Natural,
      Element_Type => Duration
   );
   use Duration_Vectors;

   -- Benchmark transformations
   procedure Fast_Transform (Input : Integer; Output : out Integer) is
   begin
      -- Minimal computation
      Output := Input * 2 + 1;
   end Fast_Transform;

   procedure Medium_Transform (Input : Integer; Output : out Integer) is
      Result : Integer := Input;
   begin
      -- Moderate computation
      for I in 1 .. 100 loop
         Result := (Result * 3 + 7) mod 1000000;
      end loop;
      Output := Result;
   end Medium_Transform;

   procedure Heavy_Transform (Input : Integer; Output : out Integer) is
      Result : Integer := Input;
   begin
      -- Heavy computation
      for I in 1 .. 10000 loop
         Result := (Result * 3 + 7) mod 1000000;
      end loop;
      Output := Result;
   end Heavy_Transform;

   -- Instantiate async operations
   function Copy_Int (Source : Integer) return Integer is (Source);
   function Default_Int return Integer is (0);
   
   package Fast_Async_Map is new Integer_Result.Async_Map_Operations (
      New_Value_Type => Integer,
      Transform => Fast_Transform,
      Copy_New_Value => Copy_Int,
      Default_New_Value => Default_Int
   );

   package Medium_Async_Map is new Integer_Result.Async_Map_Operations (
      New_Value_Type => Integer,
      Transform => Medium_Transform,
      Copy_New_Value => Copy_Int,
      Default_New_Value => Default_Int
   );

   package Heavy_Async_Map is new Integer_Result.Async_Map_Operations (
      New_Value_Type => Integer,
      Transform => Heavy_Transform,
      Copy_New_Value => Copy_Int,
      Default_New_Value => Default_Int
   );

   -- Calculate percentile from sorted latencies
   function Calculate_Percentile (Latencies : Vector; Percentile : Float) return Duration is
      Index : Natural;
   begin
      if Is_Empty (Latencies) then
         return 0.0;
      end if;
      
      Index := Natural (Float (Natural (Length (Latencies)) - 1) * Percentile / 100.0);
      return Element (Latencies, Index);
   end Calculate_Percentile;

   -- Sort latencies for percentile calculations
   procedure Sort_Latencies (Latencies : in out Vector) is
      procedure Swap (I, J : Natural) is
         Temp : constant Duration := Element (Latencies, I);
      begin
         Replace_Element (Latencies, I, Element (Latencies, J));
         Replace_Element (Latencies, J, Temp);
      end Swap;
      
      N : constant Natural := Natural (Length (Latencies));
   begin
      -- Simple bubble sort (sufficient for benchmark data)
      for I in 0 .. N - 2 loop
         for J in 0 .. N - 2 - I loop
            if Element (Latencies, J) > Element (Latencies, J + 1) then
               Swap (J, J + 1);
            end if;
         end loop;
      end loop;
   end Sort_Latencies;

   -- Run single-threaded benchmark
   procedure Run_Sequential_Benchmark (
      Config : Benchmark_Config;
      Metrics : out Performance_Metrics;
      Use_Fast : Boolean := True;
      Use_Medium : Boolean := False
   ) is
      Start_Time, Op_Start, Op_End : Time;
      Total_Time : Duration;
      Latencies : Vector;
      Success_Count : Natural := 0;
      
   begin
      Clear (Latencies);
      
      -- Warmup phase
      for I in 1 .. Config.Warmup_Iterations loop
         declare
            Input_Result : Integer_Result.Result_Type;
            Async_Op_Fast : Fast_Async_Map.Async_Map_Result_Type;
            Async_Op_Medium : Medium_Async_Map.Async_Map_Result_Type;
            Async_Op_Heavy : Heavy_Async_Map.Async_Map_Result_Type;
            Final_Result : Integer_Result.Result_Type;
         begin
            Integer_Result.Make_Ok (Input_Result, I);
            
            if Use_Fast then
               Fast_Async_Map.Start_Async_Map (Input_Result, Async_Op_Fast);
               Fast_Async_Map.Wait_For_Result (Async_Op_Fast, Final_Result);
            elsif Use_Medium then
               Medium_Async_Map.Start_Async_Map (Input_Result, Async_Op_Medium);
               Medium_Async_Map.Wait_For_Result (Async_Op_Medium, Final_Result);
            else
               Heavy_Async_Map.Start_Async_Map (Input_Result, Async_Op_Heavy);
               Heavy_Async_Map.Wait_For_Result (Async_Op_Heavy, Final_Result);
            end if;
         end;
      end loop;
      
      -- Actual benchmark
      Start_Time := Clock;
      
      for I in 1 .. Config.Iterations loop
         declare
            Input_Result : Integer_Result.Result_Type;
            Async_Op_Fast : Fast_Async_Map.Async_Map_Result_Type;
            Async_Op_Medium : Medium_Async_Map.Async_Map_Result_Type;
            Async_Op_Heavy : Heavy_Async_Map.Async_Map_Result_Type;
            Final_Result : Integer_Result.Result_Type;
         begin
            Integer_Result.Make_Ok (Input_Result, I);
            Op_Start := Clock;
            
            if Use_Fast then
               Fast_Async_Map.Start_Async_Map (Input_Result, Async_Op_Fast);
               Fast_Async_Map.Wait_For_Result (Async_Op_Fast, Final_Result);
            elsif Use_Medium then
               Medium_Async_Map.Start_Async_Map (Input_Result, Async_Op_Medium);
               Medium_Async_Map.Wait_For_Result (Async_Op_Medium, Final_Result);
            else
               Heavy_Async_Map.Start_Async_Map (Input_Result, Async_Op_Heavy);
               Heavy_Async_Map.Wait_For_Result (Async_Op_Heavy, Final_Result);
            end if;
            
            Op_End := Clock;
            
            if Integer_Result.Is_Ok (Final_Result) then
               Success_Count := Success_Count + 1;
            end if;
            
            declare
               Latency : constant Duration := Op_End - Op_Start;
            begin
               Append (Latencies, Latency);
               if Latency < Metrics.Min_Latency then
                  Metrics.Min_Latency := Latency;
               end if;
               if Latency > Metrics.Max_Latency then
                  Metrics.Max_Latency := Latency;
               end if;
            end;
         end;
      end loop;
      
      Total_Time := Clock - Start_Time;
      
      -- Calculate metrics
      Metrics.Total_Operations := Config.Iterations;
      Metrics.Total_Duration := Total_Time;
      Metrics.Throughput := Float (Config.Iterations) / Float (Total_Time);
      Metrics.Success_Rate := Float (Success_Count) / Float (Config.Iterations) * 100.0;
      
      -- Calculate average latency
      declare
         Sum : Duration := 0.0;
      begin
         for Latency of Latencies loop
            Sum := Sum + Latency;
         end loop;
         Metrics.Avg_Latency := Sum / Config.Iterations;
      end;
      
      -- Output percentiles
      Sort_Latencies (Latencies);
      Put_Line ("  Latency percentiles:");
      Put_Line ("    P50:  " & Duration'Image (Calculate_Percentile (Latencies, 50.0) * 1000.0) & " ms");
      Put_Line ("    P90:  " & Duration'Image (Calculate_Percentile (Latencies, 90.0) * 1000.0) & " ms");
      Put_Line ("    P95:  " & Duration'Image (Calculate_Percentile (Latencies, 95.0) * 1000.0) & " ms");
      Put_Line ("    P99:  " & Duration'Image (Calculate_Percentile (Latencies, 99.0) * 1000.0) & " ms");
   end Run_Sequential_Benchmark;

   -- Run concurrent benchmark
   procedure Run_Concurrent_Benchmark (
      Config : Benchmark_Config;
      Metrics : out Performance_Metrics
   ) is
      Start_Time : Time;
      Total_Time : Duration;
      
      protected Shared_Metrics is
         procedure Add_Result (Success : Boolean; Latency : Duration);
         procedure Get_Results (
            Success_Count : out Natural;
            Total_Count : out Natural;
            Min_Lat : out Duration;
            Max_Lat : out Duration;
            Avg_Lat : out Duration
         );
      private
         Successes : Natural := 0;
         Total : Natural := 0;
         Min_Latency : Duration := Duration'Last;
         Max_Latency : Duration := 0.0;
         Sum_Latency : Duration := 0.0;
      end Shared_Metrics;
      
      protected body Shared_Metrics is
         procedure Add_Result (Success : Boolean; Latency : Duration) is
         begin
            Total := Total + 1;
            if Success then
               Successes := Successes + 1;
            end if;
            Sum_Latency := Sum_Latency + Latency;
            if Latency < Min_Latency then
               Min_Latency := Latency;
            end if;
            if Latency > Max_Latency then
               Max_Latency := Latency;
            end if;
         end Add_Result;
         
         procedure Get_Results (
            Success_Count : out Natural;
            Total_Count : out Natural;
            Min_Lat : out Duration;
            Max_Lat : out Duration;
            Avg_Lat : out Duration
         ) is
         begin
            Success_Count := Successes;
            Total_Count := Total;
            Min_Lat := Min_Latency;
            Max_Lat := Max_Latency;
            if Total > 0 then
               Avg_Lat := Sum_Latency / Total;
            else
               Avg_Lat := 0.0;
            end if;
         end Get_Results;
      end Shared_Metrics;
      
      task type Benchmark_Worker is
         entry Start (Worker_Id : Natural; Operations : Natural);
      end Benchmark_Worker;
      
      task body Benchmark_Worker is
         Id : Natural;
         Op_Count : Natural;
      begin
         accept Start (Worker_Id : Natural; Operations : Natural) do
            Id := Worker_Id;
            Op_Count := Operations;
         end Start;
         
         for I in 1 .. Op_Count loop
            declare
               Input_Result : Integer_Result.Result_Type;
               Async_Op : Fast_Async_Map.Async_Map_Result_Type;
               Final_Result : Integer_Result.Result_Type;
               Op_Start, Op_End : Time;
               Success : Boolean;
            begin
               Integer_Result.Make_Ok (Input_Result, Id * 1000 + I);
               Op_Start := Clock;
               
               Fast_Async_Map.Start_Async_Map (Input_Result, Async_Op);
               Fast_Async_Map.Wait_For_Result (Async_Op, Final_Result);
               
               Op_End := Clock;
               Success := Integer_Result.Is_Ok (Final_Result);
               
               Shared_Metrics.Add_Result (Success, Op_End - Op_Start);
            end;
         end loop;
      end Benchmark_Worker;
      
      type Worker_Array is array (1 .. Config.Concurrent_Tasks) of Benchmark_Worker;
      Workers : Worker_Array;
      Operations_Per_Worker : constant Natural := Config.Iterations / Config.Concurrent_Tasks;
      
   begin
      Start_Time := Clock;
      
      -- Start all workers
      for I in Workers'Range loop
         Workers (I).Start (I, Operations_Per_Worker);
      end loop;
      
      -- Wait for completion (workers complete automatically)
      delay 0.1; -- Small delay to ensure all tasks finish
      
      Total_Time := Clock - Start_Time;
      
      -- Gather results
      declare
         Success_Count, Total_Count : Natural;
         Min_Lat, Max_Lat, Avg_Lat : Duration;
      begin
         Shared_Metrics.Get_Results (Success_Count, Total_Count, Min_Lat, Max_Lat, Avg_Lat);
         
         Metrics.Total_Operations := Total_Count;
         Metrics.Total_Duration := Total_Time;
         Metrics.Min_Latency := Min_Lat;
         Metrics.Max_Latency := Max_Lat;
         Metrics.Avg_Latency := Avg_Lat;
         Metrics.Throughput := Float (Total_Count) / Float (Total_Time);
         Metrics.Success_Rate := Float (Success_Count) / Float (Total_Count) * 100.0;
      end;
   end Run_Concurrent_Benchmark;

   -- Print metrics
   procedure Print_Metrics (Config : Benchmark_Config; Metrics : Performance_Metrics) is
   begin
      Put_Line ("Benchmark: " & To_String (Config.Name));
      Put_Line ("  Operations:     " & Natural'Image (Metrics.Total_Operations));
      Put_Line ("  Total Duration: " & Duration'Image (Metrics.Total_Duration) & " seconds");
      Put_Line ("  Throughput:     " & Float'Image (Metrics.Throughput) & " ops/sec");
      Put_Line ("  Success Rate:   " & Float'Image (Metrics.Success_Rate) & "%");
      Put_Line ("  Latency:");
      Put_Line ("    Min:  " & Duration'Image (Metrics.Min_Latency * 1000.0) & " ms");
      Put_Line ("    Avg:  " & Duration'Image (Metrics.Avg_Latency * 1000.0) & " ms");
      Put_Line ("    Max:  " & Duration'Image (Metrics.Max_Latency * 1000.0) & " ms");
   end Print_Metrics;

   -- Benchmark configurations
   Sequential_Fast : constant Benchmark_Config := (
      Name => To_Unbounded_String ("Sequential Fast Transform"),
      Iterations => 10000,
      Concurrent_Tasks => 1,
      Warmup_Iterations => 100
   );

   Sequential_Medium : constant Benchmark_Config := (
      Name => To_Unbounded_String ("Sequential Medium Transform"),
      Iterations => 1000,
      Concurrent_Tasks => 1,
      Warmup_Iterations => 10
   );

   Sequential_Heavy : constant Benchmark_Config := (
      Name => To_Unbounded_String ("Sequential Heavy Transform"),
      Iterations => 100,
      Concurrent_Tasks => 1,
      Warmup_Iterations => 5
   );

   Concurrent_2_Tasks : constant Benchmark_Config := (
      Name => To_Unbounded_String ("Concurrent Fast Transform (2 tasks)"),
      Iterations => 10000,
      Concurrent_Tasks => 2,
      Warmup_Iterations => 100
   );

   Concurrent_4_Tasks : constant Benchmark_Config := (
      Name => To_Unbounded_String ("Concurrent Fast Transform (4 tasks)"),
      Iterations => 10000,
      Concurrent_Tasks => 4,
      Warmup_Iterations => 100
   );

   Concurrent_8_Tasks : constant Benchmark_Config := (
      Name => To_Unbounded_String ("Concurrent Fast Transform (8 tasks)"),
      Iterations => 10000,
      Concurrent_Tasks => 8,
      Warmup_Iterations => 100
   );

   Metrics : Performance_Metrics;

begin
   Put_Line ("Ada Async_Result Library - Performance Benchmarks");
   Put_Line ("=================================================");
   Put_Line ("");

   -- Initialize random generator
   Ada.Numerics.Float_Random.Reset (Float_Gen);

   -- Sequential benchmarks
   Put_Line ("Running Sequential Benchmarks...");
   Put_Line ("");
   
   Run_Sequential_Benchmark (Sequential_Fast, Metrics, Use_Fast => True, Use_Medium => False);
   Print_Metrics (Sequential_Fast, Metrics);
   Put_Line ("");
   
   Run_Sequential_Benchmark (Sequential_Medium, Metrics, Use_Fast => False, Use_Medium => True);
   Print_Metrics (Sequential_Medium, Metrics);
   Put_Line ("");
   
   Run_Sequential_Benchmark (Sequential_Heavy, Metrics, Use_Fast => False, Use_Medium => False);
   Print_Metrics (Sequential_Heavy, Metrics);
   Put_Line ("");

   -- Concurrent benchmarks
   Put_Line ("Running Concurrent Benchmarks...");
   Put_Line ("");
   
   Run_Concurrent_Benchmark (Concurrent_2_Tasks, Metrics);
   Print_Metrics (Concurrent_2_Tasks, Metrics);
   Put_Line ("");
   
   Run_Concurrent_Benchmark (Concurrent_4_Tasks, Metrics);
   Print_Metrics (Concurrent_4_Tasks, Metrics);
   Put_Line ("");
   
   Run_Concurrent_Benchmark (Concurrent_8_Tasks, Metrics);
   Print_Metrics (Concurrent_8_Tasks, Metrics);
   Put_Line ("");

   -- Scalability analysis
   Put_Line ("=================================================");
   Put_Line ("Scalability Analysis:");
   Put_Line ("  Compare throughput as concurrent tasks increase");
   Put_Line ("  Ideal scaling: linear increase with task count");
   Put_Line ("=================================================");

exception
   when E : others =>
      Put_Line ("ERROR in benchmark: " & Ada.Exceptions.Exception_Information (E));
end Async_Performance_Benchmark;