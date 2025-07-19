-- =============================================================================
-- Example: Using the new Async_Result package structure
-- =============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Import the prelude for common instantiations
with Async_Result.Prelude; use Async_Result.Prelude;

procedure Test_New_Structure is

   -- Example 1: Using pre-instantiated Integer results
   procedure Test_Integer_Results is
      R1 : Int_Result;
      R2 : Int_Result;
   begin
      Put_Line ("=== Testing Integer Results ===");
      
      -- Create successful result
      Integer_Result.Make_Ok (R1, 42);
      
      -- Create error result
      Integer_Result.Make_Err (R2, Make_Error (Invalid_Argument, "Value must be positive"));
      
      -- Check and extract values
      if Integer_Result.Is_Ok (R1) then
         Put_Line ("R1 value: " & Integer'Image (Integer_Result.Unwrap (R1)));
      end if;
      
      if Integer_Result.Is_Err (R2) then
         Put_Line ("R2 error: " & Error_To_String (Integer_Result.Unwrap_Err (R2)));
      end if;
      
      Put_Line ("");
   end Test_Integer_Results;

   -- Example 2: Using simplified API
   procedure Test_Simple_API is
      use Simple_Integer;
      
      R : Result;
   begin
      Put_Line ("=== Testing Simple API ===");
      
      -- Create results using simple functions
      R := Ok (100);
      Put_Line ("Value or default: " & Integer'Image (Value_Or (R, 0)));
      
      R := Err (Make_Error (Timeout_Error, "Operation timed out"));
      Put_Line ("Is error: " & Boolean'Image (Is_Err (R)));
      
      Put_Line ("");
   end Test_Simple_API;

   -- Example 3: Using async operations
   procedure Test_Async_Operations is
      Input : Int_Result;
      
      -- Transformation function
      procedure Double_Value (Input : Integer; Output : out Integer) is
      begin
         Output := Input * 2;
      end Double_Value;
      
      -- Instantiate async map for our transformation
      package Async_Double is new Async_Integer.Async_Map_Operations (
         New_Value_Type => Integer,
         Transform => Double_Value,
         Copy_New_Value => Copy_Integer,
         Default_New_Value => Default_Integer
      );
      
      Async_Handle : Async_Double.Async_Map_Result_Type;
      Result : Async_Double.New_Result_Type;
   begin
      Put_Line ("=== Testing Async Operations ===");
      
      -- Create input
      Integer_Result.Make_Ok (Input, 21);
      
      -- Start async transformation
      Async_Double.Start_Async_Map (Input, Async_Handle);
      
      -- Wait for result
      Async_Double.Wait_For_Transformed_Result (Async_Handle, Result);
      
      if Async_Double.Is_Ok (Result) then
         Put_Line ("Async result: " & Integer'Image (Async_Double.Unwrap (Result)));
      end if;
      
      Put_Line ("Execution time: " & Duration'Image (
         Async_Double.Get_Execution_Time (Async_Handle)) & " seconds");
      
      Put_Line ("");
   end Test_Async_Operations;

   -- Example 4: Using work-stealing pool
   procedure Test_Work_Pool is
      type Work_Item is record
         Value : Integer;
      end record;
      
      procedure Process_Item (Item : Work_Item; Result : out Int_Result) is
      begin
         -- Simulate some work
         delay 0.01;
         
         if Item.Value > 0 then
            Integer_Result.Make_Ok (Result, Item.Value * Item.Value);
         else
            Integer_Result.Make_Err (Result, 
               Make_Error (Invalid_Argument, "Value must be positive"));
         end if;
      end Process_Item;
      
      -- Instantiate pool
      package Work_Pool is new Integer_Result.Work_Stealing_Pool (
         Work_Item_Type => Work_Item,
         Process_Work => Process_Item
      );
      
      -- Note: This would be used as:
      -- Pool : Work_Pool.Task_Pool_Type;
      -- Work_Pool.Initialize_Pool (Pool, Worker_Count => 4);
      -- Work_Pool.Submit_Work (Pool, (Value => 10));
      
   begin
      Put_Line ("=== Work Pool Example ===");
      Put_Line ("Work pool package instantiated successfully");
      Put_Line ("");
   end Test_Work_Pool;

   -- Example 5: Using metrics
   procedure Test_Metrics is
      use Metrics;
   begin
      Put_Line ("=== Testing Metrics ===");
      
      -- Record some metrics
      Global_Metrics.Record_Result_Created (Is_Ok => True);
      Global_Metrics.Record_Task_Start;
      delay 0.1;
      Global_Metrics.Record_Task_Complete (0.1);
      
      -- Get metrics
      declare
         Async_Stats : Async_Metrics_Type := Global_Metrics.Get_Async_Metrics;
      begin
         Put_Line ("Tasks started: " & Counter_Value'Image (Async_Stats.Tasks_Started));
         Put_Line ("Tasks completed: " & Counter_Value'Image (Async_Stats.Tasks_Completed));
      end;
      
      Put_Line ("");
   end Test_Metrics;

   -- Example 6: Pattern matching
   procedure Test_Pattern_Matching is
      R : Int_Result;
      
      procedure Handle_Success (V : Integer) is
      begin
         Put_Line ("Success: " & Integer'Image (V));
      end Handle_Success;
      
      procedure Handle_Error (E : Error_Info; Message : String) is
      begin
         Put_Line ("Error: " & Error_To_String (E));
         if Message /= "" then
            Put_Line ("Message: " & Message);
         end if;
      end Handle_Error;
      
      procedure Match_Result is new Integer_Result.Match (
         Handle_Ok => Handle_Success,
         Handle_Err => Handle_Error
      );
   begin
      Put_Line ("=== Testing Pattern Matching ===");
      
      Integer_Result.Make_Ok (R, 42);
      Match_Result (R);
      
      Integer_Result.Make_Err (R, Make_Error (Parse_Error, "Invalid format"));
      Match_Result (R);
      
      Put_Line ("");
   end Test_Pattern_Matching;

begin
   Put_Line ("Testing New Async_Result Package Structure");
   Put_Line ("=========================================");
   Put_Line ("");
   
   Test_Integer_Results;
   Test_Simple_API;
   Test_Async_Operations;
   Test_Work_Pool;
   Test_Metrics;
   Test_Pattern_Matching;
   
   Put_Line ("All tests completed successfully!");
end Test_New_Structure;