# Concurrency Implementation Guide

**Version:** 1.0.0  
**Date:** October 28, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  


## Overview

This Ada application implements comprehensive concurrency to ensure all I/O operations run on separate tasks from the main thread, providing improved responsiveness and scalability.

## Concurrent Components

### 1. Console Output (`TTY_Writer`)
- **Location**: `Hybrid.Infrastructure.Adapter.Console_Output`
- **Purpose**: Handles all console output on a dedicated task
- **Features**:
  - Protected object for thread-safe writes
  - Asynchronous message sending
  - Graceful shutdown support

### 2. Concurrent Logger
- **Location**: `Hybrid.Infrastructure.Logger.Concurrent`
- **Purpose**: Performs all logging operations on a dedicated task
- **Features**:
  - Non-blocking log calls with timeout
  - Priority-based log levels
  - Thread-safe message queue
  - Automatic timestamp formatting

### 3. Greeting Processor Task
- **Location**: `Hybrid.Application.Service.Create_Greeting_Concurrent`
- **Purpose**: Processes greeting requests asynchronously
- **Features**:
  - Request/response pattern
  - Async and sync execution modes
  - Callback support for async operations

### 4. Task Supervisor
- **Location**: `Hybrid.Infrastructure.Task_Supervisor`
- **Purpose**: Coordinates task lifecycle and shutdown
- **Features**:
  - Global task registry
  - Graceful shutdown coordination
  - Active task counting
  - RAII-based task registration

## Architecture Patterns

### Task Communication
- Tasks use rendezvous (entry/accept) for synchronous communication
- Protected objects ensure thread-safe data access
- Message passing prevents shared state issues

### Shutdown Coordination
1. Signal handler triggers shutdown request
2. Task supervisor notifies all registered tasks
3. Tasks complete current operations and terminate
4. Main thread waits for all tasks to finish

### Error Handling
- Each task has exception handlers to prevent crashes
- Timeouts prevent indefinite blocking
- Fallback mechanisms for task failures

## Usage Examples

### Running the Concurrent Version
```bash
# Build the concurrent version
gprbuild -P hybrid.gpr src/bootstrap/bootstrap-main-concurrent.adb

# Run with concurrent I/O
./bootstrap-main-concurrent "World"
```

### Key Benefits
1. **Non-blocking I/O**: Console output and logging never block the main application logic
2. **Scalability**: Worker pools can handle multiple requests concurrently
3. **Responsiveness**: Signal handling and shutdown are immediate
4. **Fault Tolerance**: Individual task failures don't crash the application

## Best Practices

1. **Always Register Tasks**: Use `Task_Registration` for automatic lifecycle management
2. **Handle Timeouts**: Implement timeouts for all blocking operations
3. **Graceful Shutdown**: Check `Is_Shutdown_Requested` in long-running loops
4. **Exception Safety**: Wrap task bodies in exception handlers

## Performance Considerations

- Task creation has overhead; reuse tasks via pools when possible
- Protected objects are faster than rendezvous for simple operations
- Use appropriate delays to prevent busy waiting
- Consider task priorities for time-critical operations