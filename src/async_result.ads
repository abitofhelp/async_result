-- =============================================================================
-- Ada Async_Result Library - Main Package
-- Copyright (c) 2025 A Bit of Help, Inc.
-- SPDX-License-Identifier: MIT
--
-- Root package for the Async_Result library following hybrid architecture
-- This serves as the namespace parent for all child packages
-- =============================================================================

package Async_Result is
   pragma Pure;
   
   -- This package serves as the root namespace for the library
   -- All functionality is provided through child packages:
   --
   -- Async_Result.Core    - Basic Result types and operations
   -- Async_Result.Async   - Asynchronous operations and futures
   -- Async_Result.Pool    - Work-stealing thread pool
   -- Async_Result.Metrics - Performance monitoring and metrics
   -- Async_Result.Simple  - Simplified API for common use cases
   
end Async_Result;