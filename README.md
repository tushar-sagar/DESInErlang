# DESInErlang

A Discrete Event Simulation (DES) framework written in pure Erlang, inspired by SimPy (Python). This project simulates bots navigating a 2D grid using the A* pathfinding algorithm.

## Features

- **Pure Erlang Implementation** - No external dependencies
- **OTP-compliant** - Uses gen_server, supervisors, and application behaviors
- **Concurrent Bots** - Each bot runs as an independent Erlang process
- **A* Pathfinding** - 4-directional movement with optimal path calculation
- **Configurable** - Grid size, number of bots, simulation duration
- **Fast Execution** - Runs as fast as possible (not real-time constrained)

## Architecture

```
des_sim_sup (one_for_all)
├── des_grid          - Grid state management (ETS)
├── des_scheduler     - DES event engine & time management
└── des_bot_sup (simple_one_for_one)
    ├── bot_1         - Individual bot process
    ├── bot_2
    └── ... bot_N
```

### Modules

| Module | Description |
|--------|-------------|
| `des_sim_app` | OTP application entry point |
| `des_sim_sup` | Main supervisor |
| `des_scheduler` | DES event queue, simulation time management |
| `des_grid` | Grid state using ETS, position management |
| `des_astar` | A* pathfinding algorithm (4-directional) |
| `des_bot` | Bot process (gen_server) |
| `des_bot_sup` | Bot supervisor (simple_one_for_one) |

## Configuration

Default configuration:

```erlang
#{
    grid_width => 50,              % Grid width
    grid_height => 50,             % Grid height
    num_bots => 10,                % Number of bots
    bot_speed => 1,                % 1 unit per simulation second
    simulation_duration_hrs => 1   % Run for 1 hour (simulation time)
}
```

## Quick Start

### Prerequisites

- Erlang/OTP 24+ installed
- rebar3 installed

### Build

```bash
rebar3 compile
```

### Run with Default Config

```bash
rebar3 shell
```

Then in the Erlang shell:

```erlang
des_sim_app:run().
```

### Run with Custom Config

```erlang
des_sim_app:run(#{
    num_bots => 20,
    simulation_duration_hrs => 2,
    grid_width => 100,
    grid_height => 100
}).
```

## Output

The simulation outputs progress to the console:

```
========================================
DES Simulation - Erlang
========================================
Configuration:
  Grid size: 50x50
  Number of bots: 10
  Bot speed: 1 unit/sec
  Duration: 1 hour(s)
========================================

[T=0.000] bot_1 spawned at {23,41}, target {8,15}
[T=0.000] bot_2 spawned at {5,33}, target {47,12}
...
[T=1.000] bot_1 moved {23,41} -> {22,41}
[T=1.000] bot_2 moved {5,33} -> {6,33}
...
[Progress] T=100 / 3600 seconds (2.8%)
...
[T=156.000] bot_1 reached {8,15}, new target {39,28}
...

=== Simulation Complete ===
Simulation time: 3600 seconds (1.00 hours)
Wall-clock time: 2.45 seconds
Speed ratio: 1469.4x real-time
Total events processed: 36000

=== Bot Statistics ===
bot_1: 38 trips, 3562 moves
bot_2: 41 trips, 3559 moves
...
Total trips completed: 392
```

## How It Works

1. **Initialization**: Application starts the supervision tree, creates the grid (ETS), and spawns bot processes
2. **Bot Spawning**: Each bot picks a random start position and destination, computes A* path
3. **Simulation Loop**: Scheduler advances simulation time, sends move events to all bots
4. **Bot Movement**: Each bot moves one step along its path per simulation second
5. **Destination Reached**: Bot picks new random destination, computes new path
6. **Completion**: Simulation stops when configured duration is reached, prints statistics

## Simulation Concepts

| SimPy (Python) | This Project (Erlang) |
|----------------|----------------------|
| Environment | des_scheduler (gen_server) |
| Process/Generator | des_bot (gen_server process) |
| Event Queue | Message passing + scheduler loop |
| yield/timeout | Move events dispatched by scheduler |
| Shared Resources | ETS table (des_grid) |

## Future Enhancements

- [ ] Web-based visualization (Cowboy + WebSocket)
- [ ] Obstacle support in grid
- [ ] Collision detection/avoidance
- [ ] 8-directional movement option
- [ ] Dynamic bot spawning/removal
- [ ] Event logging to file

## License

MIT
