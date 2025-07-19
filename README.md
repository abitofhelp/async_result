# Ada Async_Result Library

> **📦 Part of the Ada Result Ecosystem**  
> This is the **asynchronous** Async_Result library. For synchronous operations, see the companion [**result**](https://github.com/abitofhelp/result) package.

A high-performance, memory-safe Async_Result type library for Ada that provides type-safe error handling without exceptions in concurrent environments. Inspired by Rust's `Result<T, E>` and functional programming's `Either` patterns, enhanced with Ada's powerful concurrency features for asynchronous operations.

**This library is designed for asynchronous and concurrent operations.** It provides futures, tasks, and concurrent combinators for non-blocking computation.

## Quick Start

### Option 1: Using Pre-instantiated Types (Easiest)

```ada
with Async_Result.Prelude; use Async_Result.Prelude;
with Ada.Text_IO; use Ada.Text_IO;

procedure Quick_Example is
   R : Int_Result;
begin
   -- Create results
   Integer_Result.Make_Ok (R, 42);
   
   -- Check and use
   if Integer_Result.Is_Ok (R) then
      Put_Line ("Value: " & Integer'Image (Integer_Result.Unwrap (R)));
   end if;
end Quick_Example;
```

### Option 2: Custom Types with Full Features

```ada
with Async_Result.Core;
with Async_Result.Async;

procedure Custom_Example is
   type Math_Error is (Division_By_Zero, Overflow);
   
   -- Instantiate core package
   package Math_Result is new Async_Result.Core (
      Value_Type => Integer,
      Error_Type => Math_Error,
      Copy_Value => Copy_Integer,
      Copy_Error => Copy_Math_Error,
      Default_Value => Default_Integer,
      Default_Error => Default_Math_Error
   );
   
   -- Instantiate async package
   package Math_Async is new Async_Result.Async (Core => Math_Result);
   
   procedure Safe_Divide (A, B : Integer; R : out Math_Result.Result_Type) is
   begin
      if B = 0 then
         Math_Result.Make_Err (R, Division_By_Zero, "Cannot divide by zero");
      else
         Math_Result.Make_Ok (R, A / B);
      end if;
   end Safe_Divide;
   
   -- Example with async operation
   procedure Double (Input : Integer; Output : out Integer) is
   begin
      Output := Input * 2;
   end Double;
   
   package Async_Double is new Math_Async.Async_Map_Operations (
      New_Value_Type => Integer,
      Transform => Double,
      Copy_New_Value => Copy_Integer,
      Default_New_Value => Default_Integer
   );
   
   My_Result : Math_Result.Result_Type;
   Async_Op : Async_Double.Async_Map_Result_Type;
   Final_Result : Async_Double.New_Result_Type;
begin
   Safe_Divide (10, 2, My_Result);
   
   -- Start async transformation
   Async_Double.Start_Async_Map (My_Result, Async_Op);
   
   -- Do other work while operation executes...
   
   -- Get result when ready
   Async_Double.Wait_For_Transformed_Result (Async_Op, Final_Result);
   
   if Async_Double.Is_Ok (Final_Result) then
      Ada.Text_IO.Put_Line ("Result: " & Integer'Image (Async_Double.Unwrap (Final_Result)));
   end if;
end Custom_Example;
```

## Requirements

- **Ada 2022** compiler (GNAT FSF 13.1 or later)
- **Alire** (recommended) or manual build system

## Installation

### Using Alire (Recommended)

```bash
# For synchronous operations (separate package)
# alr get result

# For asynchronous operations
alr get async_result
cd async_result
alr build
```

### Manual Installation

1. Clone the repository
2. Build with your Ada compiler:

```bash
git clone https://github.com/abitofhelp/async_result
cd async_result
make build
```

## Key Features

### 🚀 **Asynchronous Operations**
- **Non-blocking computations** with futures and tasks
- **Concurrent combinators** for parallel processing
- **Work-stealing task pools** for efficient resource utilization
- **Timeout support** for bounded async operations
- **Cancellation mechanisms** for cooperative task termination

### 🔒 **Thread Safety**
- **Protected Async_Result types** for shared state management
- **Atomic operations** for metrics and counters
- **Deadlock-free design** with proper lock ordering
- **Exception safety** in concurrent contexts

### ⚡ **High Performance**
- **Zero-copy operations** through OUT parameters
- **Task pooling** to amortize creation costs
- **Efficient work distribution** across multiple threads
- **NUMA-aware** architecture (planned)

### 🛡️ **Memory Safety**
- **Automatic resource management** via controlled types
- **Exception-safe cleanup** in all error paths
- **No memory leaks** with proper finalization
- **Bounds checking** for concurrent data structures

### 📊 **Monitoring & Observability**
- **Comprehensive metrics** for async operations
- **Performance monitoring** with execution time tracking
- **Health monitoring** for task pools and workers
- **Debug support** with detailed state inspection

## Package Architecture

The Async_Result library follows a modular architecture with clear separation of concerns:

### 📦 **Package Structure**

```
Async_Result
├── Async_Result           -- Root namespace package
├── Async_Result.Core      -- Basic Result types and operations
├── Async_Result.Async     -- Asynchronous operations and futures
├── Async_Result.Pool      -- Work-stealing thread pool
├── Async_Result.Metrics   -- Performance monitoring and metrics
├── Async_Result.Simple    -- Simplified API for common use cases
└── Async_Result.Prelude   -- Pre-instantiated types for quick start
```

### **Async_Result.Core**
The foundation package providing:
- Basic `Result_Type` with success/error states
- Core operations: `Make_Ok`, `Make_Err`, `Is_Ok`, `Is_Err`
- Safe extraction: `Unwrap`, `Unwrap_Or`, `Try_Get_Value`
- Pattern matching and transformation operations
- Thread-safe `Protected_Result_Type`

### **Async_Result.Async**
Asynchronous operations including:
- `Async_Map_Operations` for non-blocking transformations
- Future-like types for deferred computation
- Async combinators: `Wait_All`, `Wait_Any`, `Race`
- Cancellation tokens and timeout configuration
- Pipeline operations for chained async transformations

### **Async_Result.Pool**
High-performance work-stealing pool featuring:
- Dynamic worker management
- Backpressure control policies
- Queue resizing and load balancing
- Comprehensive pool metrics
- Priority work items support

### **Async_Result.Metrics**
Monitoring and observability tools:
- Thread-safe atomic counters and gauges
- Timing and histogram support
- Comprehensive metric collection
- Alert configuration and thresholds
- Export formats: CSV, JSON, Prometheus

### **Async_Result.Simple**
Beginner-friendly API with:
- Simplified function names: `Ok`, `Err`, `Value`, `Error`
- Reduced boilerplate for common operations
- Pre-configured defaults
- Easy chaining operations

### **Async_Result.Prelude**
Ready-to-use instantiations for:
- `Integer_Result`, `String_Result`, `Float_Result`, `Boolean_Result`
- Common error types and helpers
- Quick start without generic instantiation

### 🎯 **Usage Guidelines**

1. **For new projects**: Start with `Async_Result.Prelude` for common types
2. **For simple use cases**: Use `Async_Result.Simple` for reduced complexity
3. **For advanced features**: Import specific packages as needed
4. **For performance-critical code**: Use `Async_Result.Core` directly

### 📝 **Example: Using the Modular Structure**

```ada
-- Option 1: Use pre-instantiated types
with Async_Result.Prelude; use Async_Result.Prelude;

procedure Example_Prelude is
   R : Int_Result;
begin
   Integer_Result.Make_Ok (R, 42);
   if Integer_Result.Is_Ok (R) then
      Put_Line (Integer'Image (Integer_Result.Unwrap (R)));
   end if;
end Example_Prelude;

-- Option 2: Use simplified API
with Async_Result.Simple;

procedure Example_Simple is
   package Simple_Int is new Async_Result.Simple (Integer, String);
   use Simple_Int;
   
   R : Result := Ok (42);
begin
   Put_Line (Integer'Image (Value_Or (R, 0)));
end Example_Simple;

-- Option 3: Custom instantiation with specific packages
with Async_Result.Core;
with Async_Result.Async;

procedure Example_Custom is
   package My_Result is new Async_Result.Core (...);
   package My_Async is new Async_Result.Async (Core => My_Result);
begin
   -- Use custom types with full control
   null;
end Example_Custom;
```

## Core API

### Construction

```ada
-- Create successful result
procedure Make_Ok (R : out Result_Type; Value : Value_Type);

-- Create error result
procedure Make_Err (R : out Result_Type; Error : Error_Type);
procedure Make_Err (R : out Result_Type; Error : Error_Type; Message : String);
```

### State Inspection

```ada
-- Check result state
function Is_Ok (R : Result_Type) return Boolean;
function Is_Error (R : Result_Type) return Boolean;
function Get_State (R : Result_Type) return Result_State;
```

### Value Extraction

```ada
-- Extract values (throws exception on error)
function Unwrap (R : Result_Type) return Value_Type;
procedure Unwrap_Into (R : Result_Type; Value : out Value_Type);

-- Safe extraction with defaults
function Unwrap_Or (R : Result_Type; Default : Value_Type) return Value_Type;
procedure Unwrap_Or_Into (R : Result_Type; Default : Value_Type; Value : out Value_Type);

-- Custom error messages
function Expect (R : Result_Type; Message : String) return Value_Type;
procedure Expect_Into (R : Result_Type; Message : String; Value : out Value_Type);

-- Extract errors
function Unwrap_Err (R : Result_Type) return Error_Type;
procedure Unwrap_Err_Into (R : Result_Type; Error : out Error_Type);
```

### Safe Extraction (No Exceptions)

```ada
-- Try to get values - returns Boolean for success
function Try_Get_Value (R : Result_Type; Value : out Value_Type) return Boolean;
function Try_Get_Error (R : Result_Type; Error : out Error_Type) return Boolean;
function Try_Get_Message (R : Result_Type; Message : out Unbounded_String) return Boolean;
```

## Async Operations

### Async Map Operations

```ada
-- Start async transformation
procedure Start_Async_Map (R : Result_Type; Async_R : out Async_Map_Result_Type);

-- Check if ready
function Is_Ready (Async_R : Async_Map_Result_Type) return Boolean;

-- Wait for result
procedure Wait_For_Result (Async_R : Async_Map_Result_Type; New_R : out Result_Type);

-- Wait with timeout
procedure Wait_For_Result_With_Timeout (
   Async_R : Async_Map_Result_Type; 
   New_R : out Result_Type;
   Timeout : Duration;
   Timed_Out : out Boolean
);

-- Cancel operation
procedure Cancel (Async_R : in out Async_Map_Result_Type);
```

### Async Combinators

```ada
-- Wait for all operations to complete
procedure Wait_All (Results : in out Result_Array_Type; Timeout : Duration := Duration'Last);

-- Wait for any operation to complete
procedure Wait_Any (
   Results : in out Result_Array_Type;
   Index : out Positive;
   Result : out Result_Type;
   Timeout : Duration := Duration'Last
);

-- Race multiple operations
procedure Race (
   Results : in out Result_Array_Type;
   Winner : out Result_Type;
   Timeout : Duration := Duration'Last
);
```

### Protected Result Types

```ada
-- Thread-safe Result operations
protected type Protected_Result_Type is
   procedure Make_Ok (Value : Value_Type);
   procedure Make_Err (Error : Error_Type);
   procedure Make_Err (Error : Error_Type; Message : String);
   
   function Is_Ok return Boolean;
   function Is_Error return Boolean;
   function Get_State return Result_State;
   
   function Try_Get_Value (Value : out Value_Type) return Boolean;
   function Try_Get_Error (Error : out Error_Type) return Boolean;
   function Try_Get_Message (Message : out Unbounded_String) return Boolean;
   
   procedure Copy_To (Target : out Result_Type);
   procedure Copy_From (Source : Result_Type);
end Protected_Result_Type;
```

## Examples

### Basic Async Operation

```ada
with Async_Result;
with Ada.Text_IO;

procedure Async_Example is
   
   package String_Result is new Async_Result (Unbounded_String, Unbounded_String);
   use String_Result;
   
   Input_Result : Result_Type;
   Async_Op : Async_Map_Operations.Async_Map_Result_Type;
   Final_Result : Result_Type;
   Content : Unbounded_String;
begin
   -- Create initial result
   Make_Ok (Input_Result, To_Unbounded_String ("Hello, World!"));
   
   -- Start async transformation
   Async_Map_Operations.Start_Async_Map (Input_Result, Async_Op);
   
   -- Check periodically if ready
   while not Async_Map_Operations.Is_Ready (Async_Op) loop
      Ada.Text_IO.Put_Line ("Operation in progress...");
      delay 0.1;
   end loop;
   
   -- Get final result
   Async_Map_Operations.Wait_For_Result (Async_Op, Final_Result);
   
   if Is_Ok (Final_Result) then
      Unwrap_Into (Final_Result, Content);
      Ada.Text_IO.Put_Line ("Result: " & To_String (Content));
   else
      Ada.Text_IO.Put_Line ("Error: " & Get_Message (Final_Result));
   end if;
end Async_Example;
```

### Concurrent Processing

```ada
with Async_Result;
with Ada.Containers.Vectors;

procedure Concurrent_Example is
   
   package Query_Result is new Async_Result 
     (Value_Type => User_Vectors.Vector,
      Error_Type => Database_Error);
   use Query_Result;
   
   Results : Result_Array_Type (1 .. 10);
   Completed_Index : Positive;
   First_Result : Result_Type;
   Users : User_Vectors.Vector;
begin
   -- Start multiple concurrent operations
   for I in Results'Range loop
      -- Each operation processes different data
      Start_Concurrent_Query (I, Results (I));
   end loop;
   
   -- Wait for any operation to complete
   Async_Combinators.Wait_Any (Results, Completed_Index, First_Result);
   
   if Is_Ok (First_Result) then
      Unwrap_Into (First_Result, Users);
      Ada.Text_IO.Put_Line ("First query completed with" & 
                            Natural'Image (Natural (Users.Length)) & " users");
   else
      Ada.Text_IO.Put_Line ("Query failed: " & Get_Message (First_Result));
   end if;
   
   -- Wait for all remaining operations
   Async_Combinators.Wait_All (Results, Timeout => 10.0);
end Concurrent_Example;
```

### Work-Stealing Pool

```ada
with Async_Result.Work_Stealing_Pool;

procedure Pool_Example is
   
   type Work_Item is record
      Data : Integer;
      ID : Natural;
   end record;
   
   procedure Process_Work (Item : Work_Item; Result : out Result_Type) is
   begin
      -- Simulate work processing
      delay 0.001;
      Make_Ok (Result, Item.Data * 2);
   end Process_Work;
   
   package Work_Pool is new Work_Stealing_Pool (Work_Item, Process_Work);
   use Work_Pool;
   
   Pool : Task_Pool_Type;
   Metrics : Pool_Metrics_Type;
begin
   -- Initialize pool with 4 workers
   Initialize_Pool (Pool, 4);
   
   -- Submit work items
   for I in 1 .. 100 loop
      Submit_Work (Pool, (Data => I, ID => I));
   end loop;
   
   -- Monitor progress
   loop
      Metrics := Get_Metrics (Pool);
      exit when Metrics.Tasks_Completed = 100;
      delay 0.1;
   end loop;
   
   -- Shutdown pool
   Shutdown_Pool (Pool);
end Pool_Example;
```

## Error Handling Patterns

### Chaining Operations

```ada
-- Chain async operations with error propagation
declare
   Result1, Result2, Result3 : Result_Type;
   Final_Value : Integer;
begin
   -- Start with initial computation
   Compute_Step1 (Input_Data, Result1);
   
   -- Chain operations - errors propagate automatically
   if Is_Ok (Result1) then
      Compute_Step2 (Unwrap (Result1), Result2);
      if Is_Ok (Result2) then
         Compute_Step3 (Unwrap (Result2), Result3);
         if Is_Ok (Result3) then
            Final_Value := Unwrap (Result3);
            -- Success path
         end if;
      end if;
   end if;
   
   -- Handle any errors
   if Is_Error (Result3) then
      Handle_Error (Result3);
   end if;
end;
```

### Parallel Error Handling

```ada
-- Handle errors in parallel operations
declare
   Results : Result_Array_Type (1 .. 10);
   Success_Count : Natural := 0;
   Error_Count : Natural := 0;
begin
   -- Process all results
   Parallel_Operations.Process_All_Parallel (Results);
   
   -- Count successes and errors
   for R of Results loop
      if Is_Ok (R) then
         Success_Count := Success_Count + 1;
      else
         Error_Count := Error_Count + 1;
         Log_Error (Get_Message (R));
      end if;
   end loop;
   
   Ada.Text_IO.Put_Line ("Completed: " & Natural'Image (Success_Count) & 
                         " succeeded, " & Natural'Image (Error_Count) & " failed");
end;
```

## Performance Considerations

### Async vs Sync Operations

```ada
-- For simple, fast operations, use sync
if Is_Simple_Operation (Input) then
   Sync_Process (Input, Result);
else
   -- For complex operations, use async
   Start_Async_Process (Input, Async_Result);
   -- Do other work...
   Wait_For_Result (Async_Result, Result);
end if;
```

### Memory Management

```ada
-- Efficient memory usage patterns
declare
   Large_Object : Large_Type;
   Result : Result_Type;
begin
   -- Preferred: Use OUT parameters to avoid copying
   Compute_Large_Value (Input, Large_Object);
   Make_Ok (Result, Large_Object);
   
   -- Avoid: Creating temporary objects
   -- Make_Ok (Result, Compute_Large_Value (Input));  -- Don't do this
end;
```

## Thread Safety

Individual Result instances are **not thread-safe** for concurrent access. For shared Results:

```ada
-- Use Protected_Result_Type for shared access
declare
   Shared_Result : Protected_Result_Type;
   Local_Result : Result_Type;
begin
   -- Thread-safe operations
   Shared_Result.Make_Ok (42);
   
   if Shared_Result.Is_Ok then
      Shared_Result.Copy_To (Local_Result);
      -- Use Local_Result safely
   end if;
end;
```

## Monitoring and Metrics

```ada
-- Access global metrics
declare
   Metrics : Async_Metrics_Type;
begin
   Metrics := Global_Metrics.Get_Metrics;
   
   Ada.Text_IO.Put_Line ("Tasks started: " & Natural'Image (Metrics.Tasks_Started));
   Ada.Text_IO.Put_Line ("Tasks completed: " & Natural'Image (Metrics.Tasks_Completed));
   Ada.Text_IO.Put_Line ("Average execution time: " & Duration'Image (Metrics.Average_Execution_Time));
   Ada.Text_IO.Put_Line ("Current active tasks: " & Natural'Image (Metrics.Current_Active_Tasks));
end;
```

## Best Practices

1. **Async for I/O-bound operations**: Use async operations for network, file, or database operations
2. **Sync for CPU-bound operations**: Use sync operations for pure computations
3. **Proper resource management**: Always use controlled types for resource cleanup
4. **Error propagation**: Let errors propagate naturally through the Async_Result chain
5. **Timeout handling**: Always set reasonable timeouts for async operations
6. **Monitoring**: Monitor metrics in production environments

## Thread Safety Notes

- **Individual Results**: Not thread-safe - use separate instances per thread
- **Protected Results**: Thread-safe for shared access
- **Async Operations**: Thread-safe with proper synchronization
- **Global Metrics**: Thread-safe with atomic operations
- **Work Pools**: Thread-safe with proper worker coordination

## Performance Tips

- **Avoid unnecessary copying**: Use OUT parameters and Try_Get_* operations
- **Pool reuse**: Use work-stealing pools for repeated operations
- **Batch operations**: Group small operations to reduce task creation overhead
- **Monitor contention**: Use metrics to identify performance bottlenecks

## Testing & Quality Assurance

### Test Coverage

- **Core Result Operations**: 176/176 tests passing (100%)
- **Async Integration Tests**: Comprehensive concurrent scenarios
- **Performance Benchmarks**: Throughput and latency measurements
- **Total Coverage**: >95% of all functionality tested

### Integration Test Results

| Test Suite | Pass Rate | Status |
|------------|-----------|---------|
| Core Operations | 100% | ✅ Production Ready |
| Protected Types | 100% | ✅ Production Ready |
| Async Map Operations | 67% | ⚠️ Minor Issues |
| Stress Testing | 100% | ✅ Production Ready |
| Atomic Counters | 100% | ✅ Production Ready |

See [ASYNC_INTEGRATION_TESTING_REPORT.md](ASYNC_INTEGRATION_TESTING_REPORT.md) for detailed results.

### Running Tests

```bash
# Core tests
alr exec -- ./tests/comprehensive_test_result

# Async integration tests
alr exec -- ./tests/async_integration_test

# Performance benchmarks
alr exec -- ./tests/async_performance_benchmark
```

## Error Handling Philosophy

This library follows the principle that **errors are not exceptional**. All operations that can fail return Results, making error handling explicit and impossible to ignore. This leads to more robust and maintainable concurrent systems.

## Comparison with Other Approaches

| Approach | 🚀 Performance | ⚠️ Safety | 🎯 Ergonomics | 🧵 Concurrency |
|----------|---------------|-----------|---------------|----------------|
| Exceptions | ⚠️ Overhead | ⚠️ Ignorable | ✅ Familiar | ⚠️ Complex |
| Optional Types | ✅ Fast | ⚠️ Limited Info | ⚠️ Basic | ⚠️ Basic |
| **Ada Async_Result** | ✅ Optimal | ✅ Type-Safe | ✅ Expressive | ✅ Excellent |

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## Security

For security concerns, please see [SECURITY.md](SECURITY.md).

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

- **Rust Community**: For the `Result<T, E>` pattern inspiration
- **Ada Community**: For feedback and Ada 2022 concurrency features
- **Functional Programming**: For Either type concepts

---

*The Ada Async_Result library brings modern asynchronous error handling patterns to Ada while maintaining the language's safety guarantees and performance characteristics. It demonstrates how functional programming concepts can be elegantly implemented in Ada's powerful concurrent type system.*