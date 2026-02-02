# DES in Erlang - Project Summary

## Overview

A true Discrete Event Simulation (DES) framework in pure Erlang, similar to SimPy (Python). Uses an event-driven architecture where simulation time advances from event to event, not in fixed increments.

## Key Concepts

| SimPy (Python) | This Project (Erlang) |
|----------------|----------------------|
| `env.timeout(1)` | `des_scheduler:schedule_event(Time+1, ...)` |
| Event queue | `gb_trees` priority queue |
| Process/Generator | Bot gen_server |
| `env.run(until=X)` | `simulation_duration_hrs` config |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    des_sim_app                              │
│                 (OTP Application)                           │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                    des_sim_sup                              │
│              (Main Supervisor - one_for_all)                │
├─────────────────┬───────────────────┬───────────────────────┤
│                 │                   │                       │
▼                 ▼                   ▼                       │
┌─────────┐  ┌─────────────┐  ┌─────────────┐                │
│des_grid │  │des_scheduler│  │ des_bot_sup │                │
│ (ETS)   │  │(Event Queue)│  │(simple_1for1)│                │
└─────────┘  └──────┬──────┘  └──────┬──────┘                │
                    │                │                        │
                    │   ┌────────────┼────────────┐          │
                    │   ▼            ▼            ▼          │
                    │ ┌────────┐ ┌────────┐  ┌────────┐      │
                    │ │ bot_1  │ │ bot_2  │  │ bot_N  │      │
                    │ └───┬────┘ └───┬────┘  └───┬────┘      │
                    │     │          │           │           │
                    └─────┴──────────┴───────────┘           │
                      schedule_event() calls back            │
└─────────────────────────────────────────────────────────────┘
```

## Event-Driven Flow

```
1. Initialization
   ┌──────────────────────────────────────────────┐
   │ Bot spawns → schedules event at T=0          │
   │ des_scheduler:schedule_event(0.0, bot_init)  │
   └──────────────────────────────────────────────┘

2. Event Loop (True DES)
   ┌──────────────────────────────────────────────┐
   │ Scheduler: pop smallest event from gb_trees  │
   │            {Time=7.0, bot_6_move_event}      │
   └─────────────────────┬────────────────────────┘
                         │
                         ▼
   ┌──────────────────────────────────────────────┐
   │ Advance simulation time: T = 7.0             │
   │ (Time JUMPS, not increments!)                │
   └─────────────────────┬────────────────────────┘
                         │
                         ▼
   ┌──────────────────────────────────────────────┐
   │ Send event to bot_6 (synchronous call)       │
   │ Bot processes: moves one step on path        │
   └─────────────────────┬────────────────────────┘
                         │
                         ▼
   ┌──────────────────────────────────────────────┐
   │ Bot schedules next event at T=8.0            │
   │ des_scheduler:schedule_event(8.0, bot_move)  │
   └─────────────────────┬────────────────────────┘
                         │
                         ▼
   ┌──────────────────────────────────────────────┐
   │ Scheduler: pop next smallest event           │
   │ (could be T=8.0 for bot_6, or T=14.0 for    │
   │  another bot that reaches destination)       │
   └──────────────────────────────────────────────┘

3. Completion
   When next event time > end_time, stop and print stats
```

## Priority Queue Implementation

Uses Erlang's `gb_trees` (General Balanced Trees) for O(log n) operations:

```erlang
%% Event key: {Time, Counter} for ordering + uniqueness
%% Event value: #event{type, pid, data}

%% Schedule new event
Key = {Time, Counter},
NewQueue = gb_trees:insert(Key, Event, Queue)

%% Get next event (smallest time)
{{Time, _}, Event, NewQueue} = gb_trees:take_smallest(Queue)
```

## Module Descriptions

| Module | Type | Purpose |
|--------|------|---------|
| `des_sim_app` | application | Entry point, starts supervision tree |
| `des_sim_sup` | supervisor | Top-level supervisor |
| `des_scheduler` | gen_server | **Priority queue**, event dispatch, time management |
| `des_grid` | gen_server | Grid state via ETS |
| `des_astar` | module | A* pathfinding (4-directional) |
| `des_bot` | gen_server | Bot process, **schedules own events** |
| `des_bot_sup` | supervisor | Dynamic bot supervisor |

## Configuration

```erlang
#{
    grid_width => 50,
    grid_height => 50,
    num_bots => 10,
    bot_speed => 1,                % units per simulation second
    simulation_duration_hrs => 1   % stop when T > 3600
}
```

## Usage

```bash
# Build
rebar3 compile

# Run
rebar3 shell
> des_sim_app:run().

# Custom config
> des_sim_app:run(#{num_bots => 20, simulation_duration_hrs => 2}).
```

## Sample Output

```
=== DES Simulation Started (Event-Driven) ===
End time: 3600 simulation seconds (1.00 hours)
Events in queue: 10

[T=7.000] bot_6 reached {45,39}, new target {17,42}
[T=20.000] bot_2 reached {29,41}, new target {25,25}
[T=32.000] bot_4 reached {45,33}, new target {34,23}
...

=== Simulation Complete ===
Simulation time: 360 seconds (0.10 hours)
Wall-clock time: 0.047 seconds
Speed ratio: 7659.6x real-time
Total events processed: 3610
```

## Why True Event-Driven DES?

| Approach | Time Advancement | Efficiency |
|----------|------------------|------------|
| Time-stepped | T=0, T=1, T=2... (fixed) | Wastes cycles on empty ticks |
| **Event-driven** | T=0 → T=7 → T=20 → T=32 | Only processes when events occur |

Event-driven is essential when:
- Events are sparse (long gaps between activities)
- Different entities operate at different speeds
- Need accurate event ordering across entities

## File Structure

```
DESInErlang/
├── src/
│   ├── des_sim.app.src
│   ├── des_sim_app.erl      # Application
│   ├── des_sim_sup.erl      # Supervisor
│   ├── des_scheduler.erl    # Event queue (gb_trees)
│   ├── des_grid.erl         # Grid state
│   ├── des_astar.erl        # A* pathfinding
│   ├── des_bot.erl          # Bot (schedules events)
│   └── des_bot_sup.erl      # Bot supervisor
├── rebar.config
├── README.md
├── SUMMARY.md
└── .gitignore
```

## Future Enhancements

- [ ] Web visualization (Cowboy + WebSocket)
- [ ] Variable bot speeds
- [ ] Obstacle support
- [ ] Collision detection
- [ ] Event logging/replay
