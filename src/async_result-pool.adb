-- =============================================================================
-- Ada Async_Result Library - Work-Stealing Pool Body
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Implementation of work-stealing thread pool
-- =============================================================================

with Ada.Unchecked_Deallocation;

package body Async_Result.Pool is

   ----------------------------------------------------------------------------
   -- Work Queue Implementation
   ----------------------------------------------------------------------------

   protected body Work_Queue is
      
      procedure Add_Work (Item : Work_Item_Type) is
      begin
         if not Is_Shutdown and then Count < Current_Capacity then
            Queue (Tail) := Item;
            Tail := (Tail mod Current_Capacity) + 1;
            Count := Count + 1;
         end if;
      end Add_Work;

      procedure Try_Add_Work (Item : Work_Item_Type; Success : out Boolean) is
      begin
         if not Is_Shutdown and then Count < Current_Capacity then
            Queue (Tail) := Item;
            Tail := (Tail mod Current_Capacity) + 1;
            Count := Count + 1;
            Success := True;
         else
            Success := False;
         end if;
      end Try_Add_Work;

      entry Get_Work (Item : out Work_Item_Type) when Count > 0 or Is_Shutdown is
      begin
         if Count > 0 then
            Item := Queue (Head);
            Head := (Head mod Current_Capacity) + 1;
            Count := Count - 1;
         end if;
      end Get_Work;

      procedure Shutdown is
      begin
         Is_Shutdown := True;
      end Shutdown;

      function Get_Queue_Length return Natural is
      begin
         return Count;
      end Get_Queue_Length;

      function Get_Queue_Capacity return Natural is
      begin
         return Current_Capacity;
      end Get_Queue_Capacity;

      procedure Set_Backpressure_Policy (Policy : Backpressure_Policy) is
      begin
         Backpressure := Policy;
      end Set_Backpressure_Policy;

      procedure Clear is
      begin
         Count := 0;
         Head := 1;
         Tail := 1;
      end Clear;

      procedure Resize_If_Needed is
      begin
         if Dynamic_Config.Enable_Dynamic_Sizing and then
            Float (Count) > Float (Current_Capacity) * Dynamic_Config.Resize_Threshold
         then
            Resize_Queue (Natural (Float (Current_Capacity) * Dynamic_Config.Resize_Factor));
         end if;
      end Resize_If_Needed;

      procedure Resize_Queue (New_Size : Natural) is
         procedure Free is new Ada.Unchecked_Deallocation (Work_Item_Array, Work_Item_Array_Access);
         New_Queue : Work_Item_Array_Access;
         Old_Queue : Work_Item_Array_Access;
      begin
         if New_Size > Current_Capacity and then New_Size <= Dynamic_Config.Max_Queue_Size then
            New_Queue := new Work_Item_Array (1 .. New_Size);
            
            -- Copy existing items
            for I in 1 .. Count loop
               New_Queue (I) := Queue (((Head + I - 2) mod Current_Capacity) + 1);
            end loop;
            
            Old_Queue := Queue;
            Queue := New_Queue;
            Current_Capacity := New_Size;
            Head := 1;
            Tail := Count + 1;
            
            -- Safely deallocate old queue
            begin
               Free (Old_Queue);
            exception
               when others =>
                  null; -- Continue even if deallocation fails
            end;
         end if;
      end Resize_Queue;

   end Work_Queue;

   ----------------------------------------------------------------------------
   -- Pool Metrics Manager Implementation
   ----------------------------------------------------------------------------

   protected body Pool_Metrics_Manager is
      
      procedure Record_Task_Submitted is
      begin
         Metrics.Tasks_Submitted := Metrics.Tasks_Submitted + 1;
      end Record_Task_Submitted;

      procedure Record_Task_Completed (Processing_Time : Duration) is
      begin
         Metrics.Tasks_Completed := Metrics.Tasks_Completed + 1;
         Metrics.Total_Processing_Time := Metrics.Total_Processing_Time + Processing_Time;
         
         if Task_Count_For_Average = 0 then
            Task_Count_For_Average := 1;
            Metrics.Min_Processing_Time := Processing_Time;
            Metrics.Max_Processing_Time := Processing_Time;
         else
            Task_Count_For_Average := Task_Count_For_Average + 1;
            
            if Processing_Time < Metrics.Min_Processing_Time then
               Metrics.Min_Processing_Time := Processing_Time;
            end if;
            
            if Processing_Time > Metrics.Max_Processing_Time then
               Metrics.Max_Processing_Time := Processing_Time;
            end if;
         end if;
         
         Metrics.Average_Processing_Time := 
            Metrics.Total_Processing_Time / Duration (Task_Count_For_Average);
      end Record_Task_Completed;

      procedure Record_Task_Failed is
      begin
         Metrics.Tasks_Failed := Metrics.Tasks_Failed + 1;
      end Record_Task_Failed;

      procedure Record_Task_Rejected is
      begin
         Metrics.Tasks_Rejected := Metrics.Tasks_Rejected + 1;
      end Record_Task_Rejected;

      procedure Update_Worker_Count (Active, Idle : Natural) is
      begin
         Metrics.Active_Workers := Active;
         Metrics.Idle_Workers := Idle;
      end Update_Worker_Count;

      procedure Update_Queue_Stats (Length, Capacity : Natural) is
      begin
         Metrics.Queue_Length := Length;
         Metrics.Queue_Capacity := Capacity;
      end Update_Queue_Stats;

      function Get_Metrics return Pool_Metrics_Type is
      begin
         return Metrics;
      end Get_Metrics;

      procedure Reset_Metrics is
      begin
         Metrics := (others => <>);
         Task_Count_For_Average := 0;
      end Reset_Metrics;

   end Pool_Metrics_Manager;

   ----------------------------------------------------------------------------
   -- Backpressure Controller Implementation
   ----------------------------------------------------------------------------

   protected body Backpressure_Controller is
      
      procedure Configure (Policy : Backpressure_Policy; Max_Queue_Size : Natural) is
      begin
         Current_Policy := Policy;
         Max_Queue := Max_Queue_Size;
      end Configure;

      function Should_Accept_Work (Current_Queue_Size : Natural) return Boolean is
      begin
         case Current_Policy is
            when Block =>
               return Current_Queue_Size < Max_Queue;
            when Drop_Oldest | Drop_Newest =>
               return True; -- Always accept, will drop items
            when Fail_Fast =>
               return Current_Queue_Size < Max_Queue;
         end case;
      end Should_Accept_Work;

      procedure Work_Submitted is
      begin
         Current_Load := Current_Load + 1;
      end Work_Submitted;

      procedure Work_Completed is
      begin
         if Current_Load > 0 then
            Current_Load := Current_Load - 1;
         end if;
      end Work_Completed;

      function Get_Policy return Backpressure_Policy is
      begin
         return Current_Policy;
      end Get_Policy;

   end Backpressure_Controller;

   ----------------------------------------------------------------------------
   -- Pool State Manager Implementation
   ----------------------------------------------------------------------------

   protected body Pool_State_Manager is
      
      procedure Set_Paused (Value : Boolean) is
      begin
         Paused := Value;
      end Set_Paused;

      function Is_Paused return Boolean is
      begin
         return Paused;
      end Is_Paused;

      procedure Set_Shutdown (Value : Boolean) is
      begin
         Shutdown := Value;
      end Set_Shutdown;

      function Is_Shutdown return Boolean is
      begin
         return Shutdown;
      end Is_Shutdown;

   end Pool_State_Manager;

   ----------------------------------------------------------------------------
   -- Worker Task Implementation
   ----------------------------------------------------------------------------

   task body Worker_Task is
      Work : Work_Item_Type;
      Result : Core.Result_Type;
      Should_Exit : Boolean := False;
      Start_Time, End_Time : Ada.Calendar.Time;
   begin
      accept Start;
      
      loop
         if not Queue_Access.all.Is_Shutdown then
            select
               Queue_Access.all.Get_Work (Work);
               
               Start_Time := Ada.Calendar.Clock;
               
               begin
                  Process_Work (Work, Result);
                  End_Time := Ada.Calendar.Clock;
                  Metrics_Access.all.Record_Task_Completed (End_Time - Start_Time);
               exception
                  when others =>
                     Metrics_Access.all.Record_Task_Failed;
               end;
               
            or
               accept Shutdown;
               Should_Exit := True;
               
            or
               delay 0.1; -- Prevent busy waiting
            end select;
         else
            Should_Exit := True;
         end if;
         
         exit when Should_Exit;
      end loop;
   end Worker_Task;

   ----------------------------------------------------------------------------
   -- Task Pool Type Implementation
   ----------------------------------------------------------------------------

   procedure Initialize_Pool (
      Pool : in out Task_Pool_Type; 
      Worker_Count : Positive;
      Initial_Queue_Size : Natural := 1000;
      Backpressure : Backpressure_Policy := Block;
      Dynamic_Config : Dynamic_Queue_Config := (others => <>);
      Worker_Configuration : Worker_Config := (others => <>)
   ) is
   begin
      if not Pool.Is_Initialized then
         Pool.Queue.Dynamic_Config := Dynamic_Config;
         Pool.Queue.Current_Capacity := Initial_Queue_Size;
         Pool.Queue.Queue := new Work_Item_Array (1 .. Initial_Queue_Size);
         
         Pool.Backpressure.Configure (Backpressure, Initial_Queue_Size);
         
         Pool.Worker_Count := Worker_Count;
         Pool.Workers := new Worker_Array (1 .. Worker_Count);
         
         for I in 1 .. Worker_Count loop
            Pool.Workers (I) := new Worker_Task (
               Queue_Access => Pool.Queue'Access,
               Metrics_Access => Pool.Metrics'Access,
               Worker_ID => I
            );
            Pool.Workers (I).Start;
         end loop;
         
         Pool.Is_Initialized := True;
      end if;
   end Initialize_Pool;

   procedure Submit_Work (Pool : in out Task_Pool_Type; Item : Work_Item_Type) is
   begin
      if Pool.Is_Initialized and not Pool.State.Is_Shutdown then
         Pool.Metrics.Record_Task_Submitted;
         Pool.Queue.Add_Work (Item);
         Pool.Backpressure.Work_Submitted;
      end if;
   end Submit_Work;

   function Try_Submit_Work (Pool : in out Task_Pool_Type; Item : Work_Item_Type) return Boolean is
      Success : Boolean;
   begin
      if Pool.Is_Initialized and not Pool.State.Is_Shutdown then
         Pool.Queue.Try_Add_Work (Item, Success);
         if Success then
            Pool.Metrics.Record_Task_Submitted;
            Pool.Backpressure.Work_Submitted;
         else
            Pool.Metrics.Record_Task_Rejected;
         end if;
         return Success;
      else
         return False;
      end if;
   end Try_Submit_Work;

   procedure Submit_Batch (Pool : in out Task_Pool_Type; Items : Work_Item_Array) is
   begin
      for Item of Items loop
         Submit_Work (Pool, Item);
      end loop;
   end Submit_Batch;

   function Get_Metrics (Pool : Task_Pool_Type) return Pool_Metrics_Type is
   begin
      return Pool.Metrics.Get_Metrics;
   end Get_Metrics;

   procedure Pause_Pool (Pool : in out Task_Pool_Type) is
   begin
      Pool.State.Set_Paused (True);
   end Pause_Pool;

   procedure Resume_Pool (Pool : in out Task_Pool_Type) is
   begin
      Pool.State.Set_Paused (False);
   end Resume_Pool;

   procedure Shutdown_Pool (Pool : in out Task_Pool_Type) is
   begin
      if Pool.Is_Initialized then
         Pool.State.Set_Shutdown (True);
         Pool.Queue.Shutdown;
         
         -- Shutdown all workers
         for I in 1 .. Pool.Worker_Count loop
            if Pool.Workers (I) /= null then
               Pool.Workers (I).Shutdown;
            end if;
         end loop;
      end if;
   end Shutdown_Pool;

   procedure Shutdown_Pool_Gracefully (
      Pool : in out Task_Pool_Type;
      Timeout : Duration := 30.0
   ) is
   begin
      -- Implementation would wait for queue to empty before shutdown
      Shutdown_Pool (Pool);
   end Shutdown_Pool_Gracefully;

   procedure Add_Workers (Pool : in out Task_Pool_Type; Count : Positive) is
   begin
      -- Implementation would dynamically add workers
      null;
   end Add_Workers;

   procedure Remove_Workers (Pool : in out Task_Pool_Type; Count : Positive) is
   begin
      -- Implementation would dynamically remove workers
      null;
   end Remove_Workers;

   procedure Set_Worker_Count (Pool : in out Task_Pool_Type; New_Count : Positive) is
   begin
      if New_Count > Pool.Worker_Count then
         Add_Workers (Pool, New_Count - Pool.Worker_Count);
      elsif New_Count < Pool.Worker_Count then
         Remove_Workers (Pool, Pool.Worker_Count - New_Count);
      end if;
   end Set_Worker_Count;

   function Get_Queue_Length (Pool : Task_Pool_Type) return Natural is
   begin
      return Pool.Queue.Get_Queue_Length;
   end Get_Queue_Length;

   function Get_Queue_Capacity (Pool : Task_Pool_Type) return Natural is
   begin
      return Pool.Queue.Get_Queue_Capacity;
   end Get_Queue_Capacity;

   procedure Clear_Queue (Pool : in out Task_Pool_Type) is
   begin
      Pool.Queue.Clear;
   end Clear_Queue;

   procedure Set_Backpressure_Policy (Pool : in out Task_Pool_Type; Policy : Backpressure_Policy) is
   begin
      Pool.Queue.Set_Backpressure_Policy (Policy);
      Pool.Backpressure.Configure (Policy, Pool.Queue.Get_Queue_Capacity);
   end Set_Backpressure_Policy;

   function Get_Backpressure_Policy (Pool : Task_Pool_Type) return Backpressure_Policy is
   begin
      return Pool.Backpressure.Get_Policy;
   end Get_Backpressure_Policy;

   procedure Enable_Work_Stealing (
      Source_Pool : in out Task_Pool_Type;
      Target_Pool : in out Task_Pool_Type
   ) is
   begin
      -- Implementation would enable work stealing between pools
      null;
   end Enable_Work_Stealing;

   procedure Disable_Work_Stealing (
      Source_Pool : in out Task_Pool_Type;
      Target_Pool : in out Task_Pool_Type
   ) is
   begin
      -- Implementation would disable work stealing between pools
      null;
   end Disable_Work_Stealing;

   overriding procedure Finalize (Pool : in out Task_Pool_Type) is
   begin
      if Pool.Is_Initialized then
         Shutdown_Pool (Pool);
         
         -- Free worker array
         declare
            procedure Free is new Ada.Unchecked_Deallocation (Worker_Array, Worker_Array_Access);
         begin
            Free (Pool.Workers);
         exception
            when others =>
               null;
         end;
         
         Pool.Is_Initialized := False;
      end if;
   end Finalize;

end Async_Result.Pool;