-- =============================================================================
-- Ada Result Library
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- High-performance, memory-safe Result type for Ada providing type-safe error
-- handling without exceptions. Part of the Ada Result ecosystem.
--
-- For full license text, see LICENSE.md
-- For documentation, see README.md
-- =============================================================================

-- async_result.ads
-- Generic Result type package specification for robust error handling
-- 
-- This package implements a Result<T, E> type that combines the best features
-- of Rust's Result type and functional programming Either types. It provides
-- type-safe error handling without exceptions for normal control flow.
--
-- Key Benefits:
--   * Explicit error handling - errors cannot be ignored
--   * Type safety - compile-time guarantees about error handling
--   * Performance - optimized with OUT parameters to minimize copying
--   * Composability - functional operations for chaining computations
--   * Flexibility - both procedural and functional programming styles supported
--
-- Basic Usage:
--   package My_Result is new Result (Integer, String);
--   R : My_Result.Result_Type;
--   My_Result.Make_Ok (R, 42);
--   if My_Result.Is_Ok (R) then
--      Value := My_Result.Unwrap (R);
--   end if;
--
-- Advanced Usage Examples:
--   * Chain operations with And_Then for error propagation
--   * Transform values with Map operations while preserving errors
--   * Pattern match with Match_Operations for explicit handling
--   * Use safe extraction with Try_Get_* operations
--
-- Thread Safety: 
--   * Individual Result instances are NOT thread-safe for concurrent access
--   * Use Protected_Result_Type for thread-safe access to shared Result instances
--   * Async operations are provided for concurrent computation
--   * Use separate Result instances for each thread when possible
--   * This library contains no global state - all operations work on instance data
--
-- Performance: 
--   * Uses OUT parameters to avoid copying large data structures
--   * Minimizes memory allocation by using stack-based storage
--   * Compiler optimizations applied to frequently called functions
--   * Simple internal representation for fast state checking
--
-- Memory Management: 
--   * Automatic cleanup when Result instances go out of scope
--   * Uses Ada's controlled types for predictable resource management
--   * Safe handling of types that contain pointers or file handles
--   * Cleanup operations will not raise exceptions

with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Task_Identification;
with Ada.Calendar;

generic
   type Value_Type is private;
   -- The type stored when the operation succeeds

   type Error_Type is private;
   -- The type stored when the operation fails

   -- Copy functions - provide custom functions if your types need special copying
   -- For simple types like Integer, the default copying is sufficient
   with function Copy_Value (Source : Value_Type) return Value_Type is <>;
   with function Copy_Error (Source : Error_Type) return Error_Type is <>;

   -- Default constructors - these create initial values for your types
   -- Example: for Integer, this might return 0
   with function Default_Value return Value_Type is <>;
   with function Default_Error return Error_Type is <>;
package Async_Result is

   pragma Elaborate_Body (Async_Result);

   use Ada.Strings.Unbounded;

   ----------------------------------------------------------------------------
   -- CORE TYPES AND CONSTANTS
   ----------------------------------------------------------------------------

   type Result_State is (Success, Error);

   type Result_Type is new Ada.Finalization.Controlled with private;

   -- Protected wrapper for thread-safe access to Result instances
   protected type Protected_Result_Type is
      -- Thread-safe construction
      procedure Make_Ok (Value : Value_Type);
      procedure Make_Err (Error : Error_Type);
      procedure Make_Err (Error : Error_Type; Message : String);
      
      -- Thread-safe state inspection
      function Is_Ok return Boolean;
      function Is_Error return Boolean;
      function Get_State return Result_State;
      
      -- Thread-safe value extraction
      function Try_Get_Value (Value : out Value_Type) return Boolean;
      function Try_Get_Error (Error : out Error_Type) return Boolean;
      function Try_Get_Message (Message : out Unbounded_String) return Boolean;
      
      -- Thread-safe copying
      procedure Copy_To (Target : out Result_Type);
      procedure Copy_From (Source : Result_Type);
      
   private
      Internal_Result : Result_Type;
   end Protected_Result_Type;

   -- Async computation task type for concurrent operations  
   task type Async_Computation_Task is
      entry Start (Input : Value_Type);
      entry Get_Result (R : out Result_Type);
      entry Cancel;
   end Async_Computation_Task;

   -- Future-like type for async operations
   type Future_Result_Type is tagged limited private;
   
   -- Async_Result status
   type Async_Status is (Pending, Completed, Cancelled, Failed, Timed_Out);

   -- Configurable timeout settings
   type Timeout_Config is record
      Default_Operation_Timeout : Duration := 30.0;  -- 30 seconds
      Task_Startup_Timeout : Duration := 60.0;       -- 60 seconds
      Task_Shutdown_Timeout : Duration := 5.0;       -- 5 seconds
      Result_Retrieval_Timeout : Duration := 30.0;   -- 30 seconds
   end record;
   
   -- Global timeout configuration
   Global_Timeout_Config : Timeout_Config;

   ----------------------------------------------------------------------------
   -- CONSTRUCTION INTERFACE
   -- These procedures create Result instances containing either a value or an error
   ----------------------------------------------------------------------------

   -- Create a Result containing a successful value
   procedure Make_Ok (R : out Result_Type; Value : Value_Type)
     with Post => Is_Ok (R) and then Unwrap (R) = Value;
   
   -- Create a Result containing an error
   procedure Make_Err (R : out Result_Type; Error : Error_Type)
     with Post => Is_Error (R);
   
   -- Create a Result containing an error with a descriptive message
   procedure Make_Err (R : out Result_Type; Error : Error_Type; Message : String)
     with Post => Is_Error (R);

   ----------------------------------------------------------------------------
   -- STATE INSPECTION INTERFACE
   -- These functions check whether a Result contains a value or an error
   ----------------------------------------------------------------------------

   -- Returns True if the Result contains a successful value
   function Is_Ok (R : Result_Type) return Boolean;
   pragma Inline (Is_Ok);

   -- Returns True if the Result contains an error
   function Is_Error (R : Result_Type) return Boolean;
   pragma Inline (Is_Error);

   -- Returns the internal state (Success or Error)
   function Get_State (R : Result_Type) return Result_State;
   pragma Inline (Get_State);

   ----------------------------------------------------------------------------
   -- VALUE EXTRACTION INTERFACE
   -- These functions and procedures extract values from Result instances
   -- WARNING: Some of these will raise exceptions if used incorrectly
   ----------------------------------------------------------------------------

   -- Extract the value from a successful Result (raises exception if error)
   function Unwrap (R : Result_Type) return Value_Type
     with Pre => Is_Ok (R);
   
   -- Extract the value into an OUT parameter (raises exception if error)
   procedure Unwrap_Into (R : Result_Type; Value : out Value_Type)
     with Pre => Is_Ok (R);
   
   -- Extract the value, or return a default if the Result contains an error
   function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type
     with Post => (if Is_Ok (R) then Unwrap_Or'Result = Unwrap (R) else Unwrap_Or'Result = Default);
   
   -- Extract the value into an OUT parameter, or use default if error
   procedure Unwrap_Or_Into (R : Result_Type; Default : Value_Type; Value : out Value_Type)
     with Post => (if Is_Ok (R) then Value = Unwrap (R) else Value = Default);
   
   -- Extract the value with a custom error message (raises exception if error)
   function Expect (R : Result_Type; Message : String) return Value_Type
     with Pre => Is_Ok (R);
   
   -- Extract the value into an OUT parameter with custom error message
   procedure Expect_Into (R : Result_Type; Message : String; Value : out Value_Type)
     with Pre => Is_Ok (R);
   
   -- Extract the error from a failed Result (raises exception if success)
   function Unwrap_Err (R : Result_Type) return Error_Type
     with Pre => Is_Error (R);
   
   -- Extract the error into an OUT parameter (raises exception if success)
   procedure Unwrap_Err_Into (R : Result_Type; Error : out Error_Type)
     with Pre => Is_Error (R);
   
   -- Extract the error with a custom message (raises exception if success)
   function Expect_Err (R : Result_Type; Message : String) return Error_Type
     with Pre => Is_Error (R);

   ----------------------------------------------------------------------------
   -- TRANSFORMATION OPERATIONS
   -- These generic packages provide ways to transform Result values
   ----------------------------------------------------------------------------

   -- Map operation - transform the success value if present, leave errors unchanged
   -- Example: transform Result containing Integer to Result containing String
   generic
      type New_Value_Type is private;
      with procedure Transform (Input : Value_Type; Output : out New_Value_Type);
   package Map_Operations is
      type New_Result_Type is new Ada.Finalization.Controlled with private;

      procedure Map (R : Result_Type; New_R : out New_Result_Type);
      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String);
      function Is_Ok (R : New_Result_Type) return Boolean;
      function Is_Error (R : New_Result_Type) return Boolean;
      function Unwrap (R : New_Result_Type) return New_Value_Type;
      function Unwrap_Err (R : New_Result_Type) return Error_Type;

   private
      type New_Result_Type is new Ada.Finalization.Controlled with record
         State : Result_State := Error;
         Value : New_Value_Type;
         Error : Error_Type;
         Message : Unbounded_String := Null_Unbounded_String;
         Has_Explicit_Message : Boolean := False;
         Is_Initialized : Boolean := False;
      end record;

      overriding procedure Initialize (Object : in out New_Result_Type);
      overriding procedure Adjust (Object : in out New_Result_Type);
      overriding procedure Finalize (Object : in out New_Result_Type);
   end Map_Operations;

   -- And_Then operation - chain operations that might fail
   -- Use this when your transformation function can itself return a Result
   generic
      with procedure Transform (Input : Value_Type; Output : out Result_Type);
   package And_Then_Operations is
      procedure And_Then (R : Result_Type; New_R : out Result_Type);
   end And_Then_Operations;

   -- Map_Err operation - transform error values while leaving success unchanged
   -- Use this to convert one error type to another or add context to errors
   generic
      with procedure Transform (Input : Error_Type; Output : out Error_Type);
   package Map_Error_Operations is
      procedure Map_Err (R : Result_Type; New_R : out Result_Type);
   end Map_Error_Operations;

   ----------------------------------------------------------------------------
   -- PATTERN MATCHING INTERFACE
   -- These operations handle both success and error cases explicitly
   ----------------------------------------------------------------------------

   -- Pattern matching - provide handlers for both success and error cases
   -- The appropriate handler will be called based on the Result's state
   generic
      type Return_Type is private;
      with procedure On_Success (V : Value_Type; Output : out Return_Type);
      with procedure On_Error (E : Error_Type; Output : out Return_Type);
   package Match_Operations is
      procedure Match (R : Result_Type; Output : out Return_Type);
   end Match_Operations;

   -- Fold operation - convert Result to a single value by handling both cases
   -- Similar to Match but emphasizes the reduction to a single output type
   generic
      type Return_Type is private;
      with procedure Success_Transform (V : Value_Type; Output : out Return_Type);
      with procedure Error_Transform (E : Error_Type; Output : out Return_Type);
   package Fold_Operations is
      procedure Fold (R : Result_Type; Output : out Return_Type);
   end Fold_Operations;

   -- Swap operation - convert success to error and error to success
   -- Use this when you want to reverse the meaning of success/failure
   generic
      with function Value_To_Error (V : Value_Type) return Error_Type;
      with function Error_To_Value (E : Error_Type) return Value_Type;
   package Swap_Operations is
      procedure Swap (R : Result_Type; Swapped_R : out Result_Type);
   end Swap_Operations;

   -- Bind operation - same as And_Then, for functional programming style
   -- Use this when chaining operations that can fail
   generic
      with procedure Transform (Input : Value_Type; Output : out Result_Type);
   package Bind_Operations is
      procedure Bind (R : Result_Type; New_R : out Result_Type);
   end Bind_Operations;

   -- Fmap operation - same as Map, for functional programming style
   -- Transform successful values while preserving errors
   generic
      type New_Value_Type is private;
      with procedure Transform (Input : Value_Type; Output : out New_Value_Type);
   package Fmap_Operations is
      type New_Result_Type is new Ada.Finalization.Controlled with private;
      procedure Fmap (R : Result_Type; New_R : out New_Result_Type);

      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String);
      function Is_Ok (R : New_Result_Type) return Boolean;
      function Is_Error (R : New_Result_Type) return Boolean;
      function Unwrap (R : New_Result_Type) return New_Value_Type;
      function Unwrap_Err (R : New_Result_Type) return Error_Type;

   private
      type New_Result_Type is new Ada.Finalization.Controlled with record
         State : Result_State := Error;
         Value : New_Value_Type;
         Error : Error_Type;
         Message : Unbounded_String := Null_Unbounded_String;
         Has_Explicit_Message : Boolean := False;
         Is_Initialized : Boolean := False;
      end record;

      overriding procedure Initialize (Object : in out New_Result_Type);
      overriding procedure Adjust (Object : in out New_Result_Type);
      overriding procedure Finalize (Object : in out New_Result_Type);
   end Fmap_Operations;

   ----------------------------------------------------------------------------
   -- SAFE EXTRACTION INTERFACE
   -- These functions extract values without raising exceptions
   ----------------------------------------------------------------------------

   -- Check if the Result is in a valid state (properly initialized)
   function Is_Valid_State (R : Result_Type) return Boolean;
   pragma Inline (Is_Valid_State);

   -- Check if the Result has an error message
   function Has_Message (R : Result_Type) return Boolean;
   pragma Inline (Has_Message);

   -- Get the length of the error message
   function Get_Message_Length (R : Result_Type) return Natural;
   pragma Inline (Get_Message_Length);

   -- Try to get the value - returns True if successful, False if error
   function Try_Get_Value (R : Result_Type; Value : out Value_Type) return Boolean
     with Post => (if Try_Get_Value'Result then Is_Ok (R) else Is_Error (R));
   
   -- Try to get the error - returns True if successful, False if contains value
   function Try_Get_Error (R : Result_Type; Error : out Error_Type) return Boolean
     with Post => (if Try_Get_Error'Result then Is_Error (R) else Is_Ok (R));
   
   -- Try to get the error message - returns True if successful
   function Try_Get_Message (R : Result_Type; Message : out Unbounded_String) return Boolean
     with Post => (if Try_Get_Message'Result then Is_Error (R) and Has_Message (R) else True);

   ----------------------------------------------------------------------------
   -- ADVANCED OPERATIONS
   -- These operations provide more specialized functionality
   ----------------------------------------------------------------------------

   -- Lazy default evaluation - only call the default function if needed
   generic
      with function Default_Fn return Value_Type;
   package Lazy_Operations is
      function Unwrap_Or_Else (R : Result_Type) return Value_Type;
   end Lazy_Operations;

   -- Conditional transformation - transform success value or use default
   generic
      with function Transform_Fn (V : Value_Type) return Value_Type;
   package Map_Or_Operations is
      function Map_Or (R : Result_Type; Default : Value_Type) return Value_Type;
   end Map_Or_Operations;

   -- Conditional success checking - check if success AND predicate is true
   generic
      with function Predicate (V : Value_Type) return Boolean;
   package Value_Predicate_Operations is
      function Is_Ok_And (R : Result_Type) return Boolean;
   end Value_Predicate_Operations;

   -- Conditional error checking - check if error AND predicate is true
   generic
      with function Predicate (E : Error_Type) return Boolean;
   package Error_Predicate_Operations is
      function Is_Err_And (R : Result_Type) return Boolean;
   end Error_Predicate_Operations;

   -- Get the error message as a String (raises exception if no message)
   function Get_Message (R : Result_Type) return String;

   ----------------------------------------------------------------------------
   -- ASYNC AND CONCURRENCY OPERATIONS
   -- These operations provide concurrent and asynchronous processing capabilities
   ----------------------------------------------------------------------------

   -- Async Map operation - transform values concurrently
   generic
      type New_Value_Type is private;
      with procedure Transform (Input : Value_Type; Output : out New_Value_Type);
      with function Copy_New_Value (Source : New_Value_Type) return New_Value_Type is <>;
      with function Default_New_Value return New_Value_Type is <>;
   package Async_Map_Operations is
      -- Result type for the transformed value
      type New_Result_Type is new Ada.Finalization.Controlled with private;
      
      -- Async operation handle
      type Async_Map_Result_Type is new Ada.Finalization.Limited_Controlled with private;
      
      -- Operations for New_Result_Type
      procedure Make_Ok (R : out New_Result_Type; Value : New_Value_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type);
      procedure Make_Err (R : out New_Result_Type; Error : Error_Type; Message : String);
      function Is_Ok (R : New_Result_Type) return Boolean;
      function Is_Error (R : New_Result_Type) return Boolean;
      function Unwrap (R : New_Result_Type) return New_Value_Type;
      function Unwrap_Err (R : New_Result_Type) return Error_Type;
      
      -- Start async transformation
      procedure Start_Async_Map (R : Result_Type; Async_R : out Async_Map_Result_Type);
      
      -- Check if async operation is complete
      function Is_Ready (Async_R : Async_Map_Result_Type) return Boolean;
      
      -- Get result status
      function Get_Status (Async_R : Async_Map_Result_Type) return Async_Status;
      
      -- Wait for completion and get result (returns New_Result_Type)
      procedure Wait_For_Transformed_Result (Async_R : Async_Map_Result_Type; New_R : out New_Result_Type);
      
      -- Wait for completion with timeout
      procedure Wait_For_Transformed_Result_With_Timeout (
         Async_R : Async_Map_Result_Type; 
         New_R : out New_Result_Type;
         Timeout : Duration;
         Timed_Out : out Boolean
      );
      
      -- Try to get result without waiting
      function Try_Get_Transformed_Result (Async_R : Async_Map_Result_Type; New_R : out New_Result_Type) return Boolean;
      
      -- Cancel async operation
      procedure Cancel (Async_R : in out Async_Map_Result_Type);
      
      -- Get execution metrics
      function Get_Execution_Time (Async_R : Async_Map_Result_Type) return Duration;
      
   private
      -- New result type for transformed values
      type New_Result_Type is new Ada.Finalization.Controlled with record
         State : Result_State := Async_Result.Error;
         Value : New_Value_Type := Default_New_Value;
         Error : Error_Type := Default_Error;
         Message : Unbounded_String := Null_Unbounded_String;
         Has_Explicit_Message : Boolean := False;
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
      private
         Current_Status : Async_Status := Pending;
         Start_Time : Ada.Calendar.Time;
         End_Time : Ada.Calendar.Time;
         Has_Start_Time : Boolean := False;
         Has_End_Time : Boolean := False;
      end Status_Manager;
      
      task type Map_Task is
         entry Start (Input : Result_Type);
         entry Get_Result (Output : out New_Result_Type);
         entry Get_Result_With_Timeout (Output : out New_Result_Type; Timeout : Duration; Success : out Boolean);
         entry Cancel;
      end Map_Task;
      
      type Async_Map_Result_Type is new Ada.Finalization.Limited_Controlled with record
         Manager : aliased Status_Manager;
         Task_Instance : Map_Task;
         Is_Started : Boolean := False;
      end record;
      
      overriding procedure Finalize (Object : in out Async_Map_Result_Type);
   end Async_Map_Operations;

   -- Parallel processing for multiple Results
   generic
      type Result_Array_Type is array (Positive range <>) of Result_Type;
      with procedure Process_Value (Input : Value_Type; Output : out Value_Type);
   package Parallel_Operations is
      -- Process multiple Results concurrently
      procedure Process_All_Parallel (Results : in out Result_Array_Type);
      
      -- Process with configurable worker count
      procedure Process_All_Parallel (Results : in out Result_Array_Type; 
                                    Worker_Count : Positive);
      
      -- Wait for all operations to complete
      procedure Wait_All_Complete (Results : Result_Array_Type);
   end Parallel_Operations;

   -- Concurrent result aggregation
   generic
      type Result_Array_Type is array (Positive range <>) of Result_Type;
      type Accumulator_Type is private;
      with procedure Initialize_Accumulator (Acc : out Accumulator_Type);
      with procedure Combine_Value (Acc : in out Accumulator_Type; Value : Value_Type);
      with procedure Combine_Error (Acc : in out Accumulator_Type; Error : Error_Type);
   package Concurrent_Fold_Operations is
      -- Fold multiple Results concurrently
      procedure Concurrent_Fold (Results : Result_Array_Type; 
                               Final_Acc : out Accumulator_Type);
   end Concurrent_Fold_Operations;

   -- Async pipeline operations
   generic
      type Intermediate_Type is private;
      type Final_Type is private;
      with procedure First_Transform (Input : Value_Type; Output : out Intermediate_Type);
      with procedure Second_Transform (Input : Intermediate_Type; Output : out Final_Type);
   package Async_Pipeline_Operations is
      type Pipeline_Result_Type is tagged limited private;
      
      -- Start async pipeline
      procedure Start_Pipeline (R : Result_Type; Pipeline_R : out Pipeline_Result_Type);
      
      -- Wait for pipeline completion
      procedure Wait_For_Pipeline (Pipeline_R : Pipeline_Result_Type; Final_R : out Result_Type);
      
      -- Get pipeline status
      function Get_Pipeline_Status (Pipeline_R : Pipeline_Result_Type) return Async_Status;
      
   private
      task type Pipeline_Task is
         entry Start (Input : Result_Type);
         entry Get_Result (Output : out Result_Type);
         entry Cancel;
      end Pipeline_Task;
      
      type Pipeline_Result_Type is tagged limited record
         Task_Instance : Pipeline_Task;
         Status : Async_Status := Pending;
      end record;
   end Async_Pipeline_Operations;

   -- Async combinators for multiple operations
   generic
      type Result_Array_Type is array (Positive range <>) of Result_Type;
   package Async_Combinators is
      type Combinator_Result_Type is tagged limited private;
      
      -- Wait for all operations to complete
      procedure Wait_All (
         Results : in out Result_Array_Type;
         Timeout : Duration := Duration'Last
      );
      
      -- Wait for any operation to complete  
      procedure Wait_Any (
         Results : in out Result_Array_Type;
         Index : out Positive;
         Result : out Result_Type;
         Timeout : Duration := Duration'Last
      );
      
      -- Race multiple operations (first to complete wins)
      procedure Race (
         Results : in out Result_Array_Type;
         Winner_Index : out Positive;
         Result : out Result_Type;
         Timeout : Duration := Duration'Last
      );
      
   private
      type Combinator_Result_Type is tagged limited record
         Status : Async_Status := Pending;
      end record;
   end Async_Combinators;

   -- Work-stealing task pool
   generic
      type Work_Item_Type is private;
      with procedure Process_Work (Item : Work_Item_Type; Result : out Result_Type);
   package Work_Stealing_Pool is
      type Task_Pool_Type is tagged limited private;
      type Pool_Metrics_Type is record
         Tasks_Submitted : Natural := 0;
         Tasks_Completed : Natural := 0;
         Tasks_Failed : Natural := 0;
         Active_Workers : Natural := 0;
         Queue_Length : Natural := 0;
      end record;
      
      -- Create pool with specified number of workers
      procedure Initialize_Pool (Pool : out Task_Pool_Type; Worker_Count : Positive);
      
      -- Submit work to pool
      procedure Submit_Work (Pool : in out Task_Pool_Type; Item : Work_Item_Type);
      
      -- Get pool metrics
      function Get_Metrics (Pool : Task_Pool_Type) return Pool_Metrics_Type;
      
      -- Shutdown pool
      procedure Shutdown_Pool (Pool : in out Task_Pool_Type);
      
   private
      type Work_Item_Array is array (Positive range <>) of Work_Item_Type;
      
      protected type Work_Queue is
         procedure Add_Work (Item : Work_Item_Type);
         procedure Try_Add_Work (Item : Work_Item_Type; Success : out Boolean);
         entry Get_Work (Item : out Work_Item_Type);
         procedure Shutdown;
         function Get_Queue_Length return Natural;
      private
         Items : Work_Item_Array (1 .. 1000);  -- Fixed size for now
         Head : Natural := 1;
         Tail : Natural := 1;
         Count : Natural := 0;
         Is_Shutdown : Boolean := False;
      end Work_Queue;
      
      protected type Pool_Metrics_Manager is
         procedure Increment_Submitted;
         procedure Increment_Completed;
         procedure Increment_Failed;
         procedure Set_Active_Workers (Count : Natural);
         function Get_Metrics return Pool_Metrics_Type;
         procedure Reset_Metrics;
      private
         Metrics : Pool_Metrics_Type;
      end Pool_Metrics_Manager;
      
      -- Named access types for work-stealing pool components
      type Work_Queue_Access is access all Work_Queue;
      type Pool_Metrics_Access is access all Pool_Metrics_Manager;
      
      -- Worker context for accessing shared resources
      type Worker_Context_Type is record
         Queue_Access : Work_Queue_Access;
         Metrics_Access : Pool_Metrics_Access;
      end record;
      
      -- Worker task type for processing work items
      task type Worker_Task (
         Queue_Access : Work_Queue_Access;
         Metrics_Access : Pool_Metrics_Access
      ) is
         entry Start;
         entry Shutdown;
      end Worker_Task;
      
      type Worker_Task_Access is access Worker_Task;
      type Worker_Array_Type is array (1 .. 10) of Worker_Task_Access;
      
      type Task_Pool_Type is tagged limited record
         Queue : aliased Work_Queue;
         Workers : Worker_Array_Type;
         Worker_Count : Natural := 0;
         Is_Initialized : Boolean := False;
         Metrics : aliased Pool_Metrics_Manager;
      end record;
   end Work_Stealing_Pool;

   -- Atomic counter for thread-safe operations
   protected type Atomic_Counter is
      procedure Increment (New_Value : out Natural);
      procedure Decrement (New_Value : out Natural);
      procedure Add (Amount : Natural; New_Value : out Natural);
      function Get_Value return Natural;
      procedure Set_Value (Value : Natural);
      procedure Reset;
   private
      Counter : Natural := 0;
   end Atomic_Counter;

   -- Enhanced error context
   type Error_Context is record
      Operation_Name : Unbounded_String;
      Task_Id : Natural := 0;
      Thread_Id : Natural := 0;
      Timestamp : Ada.Calendar.Time := Ada.Calendar.Clock;
      Call_Stack_Depth : Natural := 0;
   end record;

   -- Async metrics and monitoring
   type Async_Metrics_Type is record
      Tasks_Started : Natural := 0;
      Tasks_Completed : Natural := 0;
      Tasks_Failed : Natural := 0;
      Tasks_Cancelled : Natural := 0;
      Tasks_Timed_Out : Natural := 0;
      Average_Execution_Time : Duration := 0.0;
      Peak_Concurrent_Tasks : Natural := 0;
      Current_Active_Tasks : Natural := 0;
      Total_Execution_Time : Duration := 0.0;
      Min_Execution_Time : Duration := Duration'Last;
      Max_Execution_Time : Duration := 0.0;
   end record;
   
   protected type Async_Metrics_Manager is
      procedure Record_Task_Start;
      procedure Record_Task_Complete (Execution_Time : Duration);
      procedure Record_Task_Failed;
      procedure Record_Task_Cancelled;
      procedure Record_Task_Timed_Out;
      function Get_Metrics return Async_Metrics_Type;
      procedure Reset_Metrics;
   private
      Tasks_Started : Natural := 0;
      Tasks_Completed : Natural := 0;
      Tasks_Failed : Natural := 0;
      Tasks_Cancelled : Natural := 0;
      Tasks_Timed_Out : Natural := 0;
      Current_Active_Tasks : Natural := 0;
      Peak_Concurrent_Tasks : Natural := 0;
      Total_Execution_Time : Duration := 0.0;
      Min_Execution_Time : Duration := Duration'Last;
      Max_Execution_Time : Duration := 0.0;
   end Async_Metrics_Manager;
   
   -- Global metrics instance for thread-safe access
   Global_Metrics : Async_Metrics_Manager;

   -- Backpressure control
   type Backpressure_Policy is (Block, Drop_Oldest, Drop_Newest, Fail_Fast);
   
   protected type Backpressure_Controller is
      procedure Configure (Policy : Backpressure_Policy; Max_Queue_Size : Natural);
      function Should_Accept_Work return Boolean;
      procedure Work_Submitted;
      procedure Work_Completed;
   private
      Current_Policy : Backpressure_Policy := Block;
      Max_Queue : Natural := 1000;
      Current_Queue_Size : Natural := 0;
   end Backpressure_Controller;

   ----------------------------------------------------------------------------
   -- DEBUGGING AND DIAGNOSTICS
   -- These functions help with debugging and internal state management
   ----------------------------------------------------------------------------

   -- Get a human-readable representation of the Result
   function To_String (R : Result_Type) return String;
   
   -- Get detailed debug information about the Result
   function To_Debug_String (R : Result_Type) return String;
   
   -- Validate that the Result is in a consistent state (raises exception if not)
   procedure Validate_State (R : Result_Type);
   
   -- Check if the Result's internal state is consistent
   function Is_State_Consistent (R : Result_Type) return Boolean;
   
   -- Clean up any resources associated with this Result
   procedure Cleanup_Resources (R : in out Result_Type);
   
   -- Check if the Result's types require deep copying
   function Requires_Deep_Copy (R : Result_Type) return Boolean;

   ----------------------------------------------------------------------------
   -- EXCEPTION TYPES
   -- These exceptions are raised when operations fail
   ----------------------------------------------------------------------------

   Result_Error : exception;        -- General Result operation error
   Invalid_State_Error : exception; -- Result is in an invalid state
   Unwrap_Error : exception;        -- Unwrap called on error Result
   Expect_Error : exception;        -- Expect called on error Result

private

   type Result_Type is new Ada.Finalization.Controlled with record
      State : Result_State := Error;
      Value : Value_Type := Default_Value;
      Error : Error_Type := Default_Error;
      Message : Unbounded_String := Null_Unbounded_String;
      Has_Explicit_Message : Boolean := False;
      Is_Initialized : Boolean := False;
      Needs_Cleanup : Boolean := False;
   end record;

   -- Private implementation of Future_Result_Type
   type Future_Result_Type is tagged limited record
      Status : Async_Status := Pending;
      Result : Result_Type;
      Task_Id : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
   end record;

   overriding procedure Initialize (Object : in out Result_Type);
   overriding procedure Adjust (Object : in out Result_Type);
   overriding procedure Finalize (Object : in out Result_Type);

   procedure Ensure_Valid_State (R : Result_Type);
   procedure Ensure_Success_State (R : Result_Type);
   procedure Ensure_Error_State (R : Result_Type);

end Async_Result;
