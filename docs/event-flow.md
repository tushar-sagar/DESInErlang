# DES Event Flow Documentation

## Overview

This document explains how the Discrete Event Simulation (DES) scheduler and bot processes work together to simulate autonomous bots navigating a 2D grid.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      des_scheduler                               │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  Priority Queue (gb_trees)                               │    │
│  │  Key: {SimTime, Counter}  Value: {EventType, Pid, Data} │    │
│  └─────────────────────────────────────────────────────────┘    │
│                              │                                   │
│                              ▼                                   │
│                    Pop smallest time                             │
│                    Dispatch to bot                               │
└─────────────────────────────────────────────────────────────────┘
                               │
            gen_server:call(Pid, {event, Type, SimTime, Data})
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                         des_bot                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  State                                                   │    │
│  │  - position: current grid cell                          │    │
│  │  - destination: target grid cell                        │    │
│  │  - path: [Step1, Step2, ..., Dest] (pending moves)      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Handles events: plan_path, book_reservation, do_move           │
│  Schedules ONE event at a time back to scheduler                │
└─────────────────────────────────────────────────────────────────┘
```

## Event Types

| Event | Purpose | Triggers |
|-------|---------|----------|
| `plan_path` | Compute A* path from current position to destination | `book_reservation` |
| `book_reservation` | Reserve next cell (for COWHCA mode) | `do_move` |
| `do_move` | Move bot to next cell, update state | `book_reservation` or `plan_path` |

## Event Flow Diagram

```
                    ┌──────────────┐
                    │  Bot Init    │
                    │  (spawn)     │
                    └──────┬───────┘
                           │
                           ▼
              schedule_event(0.0, plan_path)
                           │
                           ▼
┌──────────────────────────────────────────────────────────────────┐
│                                                                  │
│  ┌────────────┐                                                  │
│  │ plan_path  │◄─────────────────────────────────────────────┐   │
│  └─────┬──────┘                                              │   │
│        │                                                     │   │
│        │ 1. Compute A* path: [Current, S1, S2, ..., Dest]   │   │
│        │ 2. Store path = [S1, S2, ..., Dest] in state       │   │
│        │ 3. Schedule book_reservation for S1                 │   │
│        ▼                                                     │   │
│  ┌──────────────────┐                                        │   │
│  │ book_reservation │                                        │   │
│  └────────┬─────────┘                                        │   │
│           │                                                  │   │
│           │ 1. Try to reserve cell (COWHCA) or skip (local) │   │
│           │ 2. On success: schedule do_move                  │   │
│           │    On failure: schedule plan_path (replan)       │   │
│           ▼                                                  │   │
│     ┌──────────┐                                             │   │
│     │ do_move  │                                             │   │
│     └────┬─────┘                                             │   │
│          │                                                   │   │
│          │ 1. Move bot to new position                       │   │
│          │ 2. Pop head from path                             │   │
│          │ 3. If path not empty:                             │   │
│          │       schedule book_reservation for next step ────┼───┘
│          │    If path empty (reached destination):           │
│          │       schedule plan_path ─────────────────────────┘
│          ▼
│    [Continue until simulation ends]
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

## Detailed Event Lifecycle

### 1. plan_path Event

**When:** At T=0 (initial) or when bot reaches destination

**Actions:**
```erlang
1. If position == destination:
   - Generate new random destination
   - Increment trips_completed

2. Compute A* path from position to destination
   - Path = [Current, Step1, Step2, ..., Destination]

3. Store remaining steps in state.path
   - state.path = [Step1, Step2, ..., Destination]

4. Schedule ONE book_reservation event
   - Time: SimTime + (1.0 / Speed)
   - Data: #{pos => Step1}
```

### 2. book_reservation Event

**When:** Before each move (to reserve the cell in COWHCA mode)

**Actions:**
```erlang
1. Extract NextPos from event data

2. If pathfinder == cowhca:
   - Call butler_server to reserve cell
   - If conflict: schedule plan_path, return

3. Schedule do_move event at same SimTime
   - Data: #{pos => NextPos}
```

### 3. do_move Event

**When:** After successful reservation (or immediately in local mode)

**Actions:**
```erlang
1. Update bot position in grid
2. Update statistics (moves, distance)

3. Pop completed step from path:
   - RemainingPath = tl(Path)

4. Decide next event:
   - If RemainingPath == []:
       Bot reached destination
       Schedule plan_path at NextTime
   - Else:
       Schedule book_reservation for hd(RemainingPath) at NextTime
```

## State Management

### Bot State Record

```erlang
-record(state, {
    id,                    % atom: bot_1, bot_2, ...
    position,              % {X, Y}: current grid cell
    destination,           % {X, Y}: target grid cell
    path,                  % [{X,Y}, ...]: remaining steps to destination
    grid_width,            % integer: grid dimension
    grid_height,           % integer: grid dimension
    trips_completed,       % integer: count of destinations reached
    total_moves,           % integer: count of cells moved
    bot_speed,             % integer: cells per simulation second
    %% Analytics
    path_requests,         % integer: A* calls count
    total_path_length,     % integer: sum of all path lengths
    path_compute_time_us,  % integer: total A* time in microseconds
    total_distance,        % integer: manhattan distance traveled
    %% Timestamps
    start_time,            % float: first event time
    last_event_time,       % float: most recent event time
    %% Reservation support (COWHCA mode)
    base_monotonic,        % integer: monotonic time base
    reservation_failures   % integer: count of conflicts
}).
```

### Path Evolution Example

```
Initial state after plan_path:
  position = {0, 0}
  destination = {3, 2}
  path = [{1,0}, {2,0}, {3,0}, {3,1}, {3,2}]

After do_move to {1,0}:
  position = {1, 0}
  path = [{2,0}, {3,0}, {3,1}, {3,2}]

After do_move to {2,0}:
  position = {2, 0}
  path = [{3,0}, {3,1}, {3,2}]

... continues until ...

After do_move to {3,2}:
  position = {3, 2}
  path = []  <-- triggers plan_path
```

## Scheduler Queue

### Single Event Per Bot

The key design principle: **each bot has at most ONE event in the scheduler queue at any time**.

```
Old approach (bulk scheduling):
  Queue: [(T=1, bot_1, move), (T=2, bot_1, move), (T=3, bot_1, move), ...]
  Problem: Queue grows with O(bots × path_length)

New approach (chain scheduling):
  Queue: [(T=1, bot_1, book_reservation)]
  After processing: [(T=1, bot_1, do_move)]
  After processing: [(T=2, bot_1, book_reservation)]
  Benefit: Queue size = O(bots)
```

### Benefits

1. **Smaller queue** - O(num_bots) instead of O(num_bots × avg_path_length)
2. **Simpler replanning** - No stale events to cancel on path change
3. **Better locality** - Bot owns its pending moves in local state
4. **Easier conflict handling** - Just replan, no orphaned events

## Timing

### Event Timing Formula

```
Speed = 1 (cell per simulation second)

plan_path at T=0:
  → book_reservation at T=1.0

book_reservation at T=1.0:
  → do_move at T=1.0 (same time)

do_move at T=1.0:
  → book_reservation at T=2.0 (T + 1/Speed)

do_move at T=N (path empty):
  → plan_path at T=N+1
```

### Simulation Time vs Wall Clock

- Simulation time jumps discretely between events
- No fixed time step - true event-driven DES
- Wall clock time depends on computation speed

## Tracing

Enable trace logging for a specific bot:

```erlang
application:set_env(des_sim, trace_bot, bot_1).
```

Example trace output:
```
[T=0.000] bot_1 planning path from {30,43} to {47,49} (path_len=25)
[T=1.000] bot_1 book_reservation for {30,44}
[T=1.000] bot_1 do_move {30,43} -> {30,44} (remaining=24)
[T=1.000] bot_1 scheduling book_reservation for {30,45} at T=2.000
[T=2.000] bot_1 book_reservation for {30,45}
[T=2.000] bot_1 do_move {30,44} -> {30,45} (remaining=23)
...
[T=25.000] bot_1 do_move {47,49} -> {47,49} (remaining=0)
[T=25.000] bot_1 scheduling plan_path at T=26.000
[T=26.000] bot_1 reached {47,49}, new target {29,44} (path_len=25)
```

## Configuration

| Parameter | Default | Description |
|-----------|---------|-------------|
| `grid_width` | 50 | Grid width in cells |
| `grid_height` | 50 | Grid height in cells |
| `num_bots` | 10 | Number of bot processes |
| `bot_speed` | 1 | Cells per simulation second |
| `simulation_duration_hrs` | 1 | Simulation length |
| `pathfinder` | `local` | `local` (A*) or `cowhca` (remote) |
| `trace_bot` | `undefined` | Bot ID to enable trace logging |
