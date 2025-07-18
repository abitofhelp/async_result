-- Minimal test to isolate work-stealing pool hang
with Async_Result;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Minimal_Pool_Test is
   
   package Integer_Result is new Async_Result (
      Value_Type => Integer,
      Error_Type => Unbounded_String,
      Default_Value => 0,
      Default_Error => To_Unbounded_String ("ERROR")
   );
   
   type Simple_Work is record
      Value : Integer;
   end record;
   
   procedure Process_Simple_Work (Item : Simple_Work; Result : out Integer_Result.Result_Type) is
   begin
      Put_Line ("  Processing work: " & Integer'Image (Item.Value));
      Integer_Result.Make_Ok (Result, Item.Value * 2);
      Put_Line ("  Work completed: " & Integer'Image (Item.Value * 2));
   end Process_Simple_Work;
   
   package Simple_Pool is new Integer_Result.Work_Stealing_Pool (Simple_Work, Process_Simple_Work);
   
   Pool : Simple_Pool.Task_Pool_Type;
   Metrics : Simple_Pool.Pool_Metrics_Type;
   
begin
   Put_Line ("=== Minimal Pool Test ===");
   
   -- Step 1: Initialize pool
   Put_Line ("Step 1: Initializing pool with 1 worker...");
   Simple_Pool.Initialize_Pool (Pool, 1);
   Put_Line ("Pool initialized successfully");
   
   -- Step 2: Check initial metrics
   Metrics := Simple_Pool.Get_Metrics (Pool);
   Put_Line ("Initial metrics - Active Workers: " & Natural'Image (Metrics.Active_Workers));
   Put_Line ("Initial metrics - Queue Length: " & Natural'Image (Metrics.Queue_Length));
   
   -- Step 3: Submit one work item
   Put_Line ("Step 2: Submitting one work item...");
   Simple_Pool.Submit_Work (Pool, (Value => 42));
   Put_Line ("Work submitted");
   
   -- Step 4: Check metrics again
   Metrics := Simple_Pool.Get_Metrics (Pool);
   Put_Line ("After submit - Tasks Submitted: " & Natural'Image (Metrics.Tasks_Submitted));
   Put_Line ("After submit - Queue Length: " & Natural'Image (Metrics.Queue_Length));
   
   -- Step 5: Wait briefly for processing
   Put_Line ("Step 3: Waiting for work to be processed...");
   for I in 1 .. 10 loop
      delay 0.1;
      Metrics := Simple_Pool.Get_Metrics (Pool);
      Put_Line ("Iteration " & Integer'Image (I) & 
               " - Completed: " & Natural'Image (Metrics.Tasks_Completed) &
               " Failed: " & Natural'Image (Metrics.Tasks_Failed) &
               " Queue: " & Natural'Image (Metrics.Queue_Length));
      exit when Metrics.Tasks_Completed > 0 or Metrics.Tasks_Failed > 0;
   end loop;
   
   -- Step 6: Shutdown
   Put_Line ("Step 4: Shutting down pool...");
   Simple_Pool.Shutdown_Pool (Pool);
   Put_Line ("Pool shutdown completed");
   
   -- Final metrics
   Metrics := Simple_Pool.Get_Metrics (Pool);
   Put_Line ("Final - Completed: " & Natural'Image (Metrics.Tasks_Completed) &
            " Failed: " & Natural'Image (Metrics.Tasks_Failed));
   
   Put_Line ("=== Test Complete ===");
   
end Minimal_Pool_Test;