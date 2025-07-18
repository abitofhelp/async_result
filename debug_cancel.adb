-- Debug async map cancellation specifically
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Async_Result;

procedure Debug_Cancel is

   type Test_Error is (Network_Error, Unknown_Error);
   
   function Copy_Integer (Source : Integer) return Integer is (Source);
   function Copy_Test_Error (Source : Test_Error) return Test_Error is (Source);
   function Default_Integer return Integer is (0);
   function Default_Test_Error return Test_Error is (Unknown_Error);
   
   package Integer_Result is new Async_Result (
      Value_Type => Integer,
      Error_Type => Test_Error,
      Copy_Value => Copy_Integer,
      Copy_Error => Copy_Test_Error,
      Default_Value => Default_Integer,
      Default_Error => Default_Test_Error
   );
   use Integer_Result;

   procedure Simple_Transform (Input : Integer; Output : out Integer) is
   begin
      delay 0.1; -- Simulate work
      Output := Input * 2;
   end Simple_Transform;
   
   function Copy_New_Integer (Source : Integer) return Integer is (Source);
   function Default_New_Integer return Integer is (0);
   
   package Integer_Async_Map is new Integer_Result.Async_Map_Operations (
      New_Value_Type => Integer,
      Transform => Simple_Transform,
      Copy_New_Value => Copy_New_Integer,
      Default_New_Value => Default_New_Integer
   );

begin
   Put_Line ("=== Debug Async Map Cancellation ===");
   
   declare
      Input_Result : Integer_Result.Result_Type;
      Async_Op : Integer_Async_Map.Async_Map_Result_Type;
      Final_Result : Integer_Result.Result_Type;
   begin
      Put_Line ("Step 1: Creating input result");
      Integer_Result.Make_Ok (Input_Result, 200);
      
      Put_Line ("Step 2: Starting async map");
      Integer_Async_Map.Start_Async_Map (Input_Result, Async_Op);
      
      Put_Line ("Step 3: Status = " & Integer_Async_Map.Get_Status (Async_Op)'Image);
      
      Put_Line ("Step 4: Calling Cancel immediately");
      Integer_Async_Map.Cancel (Async_Op);
      
      Put_Line ("Step 5: Status after cancel = " & Integer_Async_Map.Get_Status (Async_Op)'Image);
      
      Put_Line ("Step 6: Calling Try_Get_Result");
      if Integer_Async_Map.Try_Get_Result (Async_Op, Final_Result) then
         Put_Line ("Step 7: Try_Get_Result returned True");
         Put_Line ("Step 8: Result status = " & Integer_Result.Get_State (Final_Result)'Image);
      else
         Put_Line ("Step 7: Try_Get_Result returned False");
      end if;
      
      Put_Line ("Step 9: Test completed");
   end;
   
   Put_Line ("=== Debug Complete ===");
   
end Debug_Cancel;