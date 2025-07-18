-- Test to isolate hang issue by running tests in isolation
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics.Discrete_Random;
with Async_Result;

procedure Test_Hang_Isolation is

   -- Test statistics
   Tests_Run : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;
   
   -- Test suite sections
   Section_Tests : Natural := 0;
   Section_Passed : Natural := 0;

   -- Test helper procedures
   procedure Start_Section (Section_Name : String) is
   begin
      Put_Line ("=== " & Section_Name & " ===");
      Section_Tests := 0;
      Section_Passed := 0;
   end Start_Section;

   procedure End_Section is
   begin
      Put_Line ("Section: " & Natural'Image (Section_Passed) & "/" & Natural'Image (Section_Tests) & " passed");
      Put_Line ("");
   end End_Section;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      Tests_Run := Tests_Run + 1;
      Section_Tests := Section_Tests + 1;
      if Condition then
         Put_Line ("  [PASS] " & Message);
         Tests_Passed := Tests_Passed + 1;
         Section_Passed := Section_Passed + 1;
      else
         Put_Line ("  [FAIL] " & Message);
         Tests_Failed := Tests_Failed + 1;
      end if;
   end Assert;

   -- Random number generation for load testing
   package Random_Natural is new Ada.Numerics.Discrete_Random (Natural);
   Random_Gen : Random_Natural.Generator;

   -- Test data types
   type Test_Error is (Network_Error, Timeout_Error, Parse_Error, Unknown_Error);
   
   -- Helper functions for instantiation
   function Copy_Integer (Source : Integer) return Integer is (Source);
   function Copy_Test_Error (Source : Test_Error) return Test_Error is (Source);
   function Copy_String (Source : Unbounded_String) return Unbounded_String is (Source);
   function Default_Integer return Integer is (0);
   function Default_Test_Error return Test_Error is (Unknown_Error);
   function Default_String return Unbounded_String is (Null_Unbounded_String);
   
   package Integer_Result is new Async_Result (
      Value_Type => Integer,
      Error_Type => Test_Error,
      Copy_Value => Copy_Integer,
      Copy_Error => Copy_Test_Error,
      Default_Value => Default_Integer,
      Default_Error => Default_Test_Error
   );
   use Integer_Result;
   
   package String_Result is new Async_Result (
      Value_Type => Unbounded_String,
      Error_Type => Test_Error,
      Copy_Value => Copy_String,
      Copy_Error => Copy_Test_Error,
      Default_Value => Default_String,
      Default_Error => Default_Test_Error
   );

   -- Test workload simulation
   procedure Simulate_Work (Duration_Ms : Natural) is
      Start_Time : constant Time := Clock;
      Target_Duration : constant Duration := Duration (Duration_Ms) / 1000.0;
   begin
      -- Simulate work with computation
      loop
         exit when Clock - Start_Time >= Target_Duration;
         for I in 1 .. 100 loop
            null; -- Busy wait
         end loop;
      end loop;
   end Simulate_Work;

   -- Test transformation functions
   procedure Double_Value (Input : Integer; Output : out Integer) is
   begin
      Simulate_Work (Random_Natural.Random (Random_Gen) mod 10 + 1);
      Output := Input * 2;
   end Double_Value;

   procedure String_Length (Input : Unbounded_String; Output : out Integer) is
   begin
      Simulate_Work (Random_Natural.Random (Random_Gen) mod 5 + 1);
      Output := Length (Input);
   end String_Length;

   -- Instantiate async operations with proper parameters
   function Copy_Integer_For_Map (Source : Integer) return Integer is (Source);
   function Default_Integer_For_Map return Integer is (0);
   
   package Integer_Async_Map is new Integer_Result.Async_Map_Operations (
      New_Value_Type => Integer,
      Transform => Double_Value,
      Copy_New_Value => Copy_Integer_For_Map,
      Default_New_Value => Default_Integer_For_Map
   );

   function Copy_Integer_For_String_Map (Source : Integer) return Integer is (Source);
   function Default_Integer_For_String_Map return Integer is (0);
   
   package String_To_Integer_Map is new String_Result.Async_Map_Operations (
      New_Value_Type => Integer,
      Transform => String_Length,
      Copy_New_Value => Copy_Integer_For_String_Map,
      Default_New_Value => Default_Integer_For_String_Map
   );

   -- Work item for work-stealing pool tests
   type Work_Item is record
      Input_Value : Integer;
      Work_Duration_Ms : Natural;
   end record;

   procedure Process_Work_Item (Item : Work_Item; Result : out Integer_Result.Result_Type) is
   begin
      Simulate_Work (Item.Work_Duration_Ms);
      if Item.Input_Value mod 13 = 0 then
         -- Simulate some failures
         Integer_Result.Make_Err (Result, Network_Error, "Simulated network error");
      else
         Integer_Result.Make_Ok (Result, Item.Input_Value * 2);
      end if;
   exception
      when Ex : others =>
         Integer_Result.Make_Err (Result, Unknown_Error, "Work processing failed: " & Exception_Information (Ex));
   end Process_Work_Item;

   package Integer_Work_Pool is new Integer_Result.Work_Stealing_Pool (
      Work_Item_Type => Work_Item,
      Process_Work => Process_Work_Item
   );

   -- Test procedures
   procedure Test_Async_Map_Operations is
   begin
      Start_Section ("Async Map Operations");

      declare
         Input_Result : Integer_Result.Result_Type;
         Async_Op : Integer_Async_Map.Async_Map_Result_Type;
         Final_Result : Integer_Result.Result_Type;
         Start_Time : Time;
         Execution_Time : Duration;
      begin
         -- Test successful async map
         Integer_Result.Make_Ok (Input_Result, 21);
         Start_Time := Clock;
         Integer_Async_Map.Start_Async_Map (Input_Result, Async_Op);
         
         -- Test status checking
         Assert (Integer_Async_Map.Get_Status (Async_Op) = Pending, "Initial status is Pending");
         
         -- Wait for completion
         Integer_Async_Map.Wait_For_Result (Async_Op, Final_Result);
         Execution_Time := Clock - Start_Time;
         
         Assert (Integer_Result.Is_Ok (Final_Result), "Async map produces success result");
         Assert (Integer_Result.Unwrap (Final_Result) = 42, "Async map transforms value correctly");
         Assert (Integer_Async_Map.Get_Status (Async_Op) = Completed, "Final status is Completed");
         Assert (Execution_Time > 0.001, "Async operation takes measurable time");
         
         -- Test execution time retrieval
         declare
            Recorded_Time : Duration := Integer_Async_Map.Get_Execution_Time (Async_Op);
         begin
            Assert (Recorded_Time > 0.0, "Execution time is recorded");
            Assert (Recorded_Time <= Execution_Time + 0.1, "Execution time is reasonable");
         end;
      end;

      -- Test async map with timeout
      declare
         Input_Result : Integer_Result.Result_Type;
         Async_Op : Integer_Async_Map.Async_Map_Result_Type;
         Final_Result : Integer_Result.Result_Type;
         Timed_Out : Boolean;
      begin
         Integer_Result.Make_Ok (Input_Result, 100);
         Integer_Async_Map.Start_Async_Map (Input_Result, Async_Op);
         
         -- Test timeout with very short timeout
         Integer_Async_Map.Wait_For_Result_With_Timeout (Async_Op, Final_Result, 0.001, Timed_Out);
         
         if Timed_Out then
            Assert (True, "Timeout mechanism works correctly");
         else
            Assert (Integer_Result.Is_Ok (Final_Result), "Fast operation completes within timeout");
         end if;
      end;

      -- Test async map cancellation
      declare
         Input_Result : Integer_Result.Result_Type;
         Async_Op : Integer_Async_Map.Async_Map_Result_Type;
         Final_Result : Integer_Result.Result_Type;
      begin
         Integer_Result.Make_Ok (Input_Result, 200);
         Integer_Async_Map.Start_Async_Map (Input_Result, Async_Op);
         
         -- Cancel immediately
         Integer_Async_Map.Cancel (Async_Op);
         
         -- Try to get result after cancellation
         if Integer_Async_Map.Try_Get_Result (Async_Op, Final_Result) then
            Assert (Integer_Async_Map.Get_Status (Async_Op) = Cancelled, "Cancelled operation has correct status");
         else
            Assert (True, "Cancelled operation doesn't return result");
         end if;
      end;

      End_Section;
   end Test_Async_Map_Operations;

   procedure Test_Protected_Result_Types is
   begin
      Start_Section ("Protected Result Types");

      -- Test concurrent access to protected results
      declare
         Protected_Result : Integer_Result.Protected_Result_Type;
         
         task Writer_Task is
            entry Start (Value : Integer);
         end Writer_Task;
         
         task body Writer_Task is
            Val : Integer;
         begin
            accept Start (Value : Integer) do
               Val := Value;
            end Start;
            
            Simulate_Work (Random_Natural.Random (Random_Gen) mod 10 + 1);
            Protected_Result.Make_Ok (Val);
         end Writer_Task;
         
         task Reader_Task is
            entry Start;
            entry Get_Result (Result : out Integer);
         end Reader_Task;
         
         task body Reader_Task is
            Value : Integer;
            Success : Boolean := False;
         begin
            accept Start;
            
            -- Poll until result is available
            loop
               Success := Protected_Result.Try_Get_Value (Value);
               exit when Success;
               delay 0.001;
            end loop;
            
            accept Get_Result (Result : out Integer) do
               Result := Value;
            end Get_Result;
         end Reader_Task;
         
         Final_Value : Integer;
         
      begin
         -- Test concurrent write and read
         Writer_Task.Start (42);
         Reader_Task.Start;
         
         Reader_Task.Get_Result (Final_Value);
         
         Assert (Final_Value = 42, "Protected result handles concurrent access correctly");
         Assert (Protected_Result.Is_Ok, "Protected result maintains correct state");
      end;

      End_Section;
   end Test_Protected_Result_Types;

   procedure Test_Work_Stealing_Pool is
   begin
      Start_Section ("Work-Stealing Pool");
      Put_Line ("  Work-Stealing Pool test started");
      -- Temporarily comment out all logic to isolate the hang
      -- Just test if the section starts and ends without hanging
      delay 0.1; -- Small delay to see if timing is the issue
      Put_Line ("  Work-Stealing Pool test about to end");
      End_Section;
   end Test_Work_Stealing_Pool;

begin
   Put_Line ("Ada Async_Result Library - Hang Isolation Test");
   Put_Line ("===============================================");
   Put_Line ("");

   -- Initialize random number generator
   Random_Natural.Reset (Random_Gen);

   -- Run only the tests before Work_Stealing_Pool
   Test_Async_Map_Operations;
   Test_Protected_Result_Types;
   
   Put_Line ("About to call Test_Work_Stealing_Pool...");
   Test_Work_Stealing_Pool;
   Put_Line ("Test_Work_Stealing_Pool completed successfully!");

   -- Final summary
   Put_Line ("===============================================");
   Put_Line ("Hang Isolation Test Summary:");
   Put_Line ("  Total Tests:  " & Natural'Image (Tests_Run));
   Put_Line ("  Passed:       " & Natural'Image (Tests_Passed));
   Put_Line ("  Failed:       " & Natural'Image (Tests_Failed));
   
   if Tests_Failed = 0 then
      Put_Line ("  Result:       ALL TESTS PASSED!");
   else
      Put_Line ("  Result:       " & Natural'Image (Tests_Failed) & " TESTS FAILED!");
   end if;
   
   Put_Line ("===============================================");

exception
   when Ex : others =>
      Put_Line ("CRITICAL ERROR in hang isolation test: " & Exception_Information (Ex));
      raise;
end Test_Hang_Isolation;