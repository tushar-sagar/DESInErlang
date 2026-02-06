# DESInErlang

Discrete Event Simulation framework in pure Erlang, inspired by SimPy. Simulates autonomous bots navigating a 2D grid using A* pathfinding with an event-driven scheduler.

## Tech Stack

- **Language**: Erlang/OTP 24+
- **Build**: Rebar3
- **Dependencies**: None (pure stdlib)
- **Storage**: ETS (in-memory), gb_trees (priority queue)
- **Optional**: COWHCA/butler_server for distributed pathfinding via RPC

## Build & Run

```bash
rebar3 compile                # Compile
rebar3 shell                  # Start shell
des_sim_app:run().            # Run with default config
des_sim_app:run(#{num_bots => 20, grid_width => 100}).  # Custom config
rebar3 as prod release        # Production release
```

## Architecture

### Supervision Tree

```
des_sim_app (application)
  └── des_sim_sup (supervisor, one_for_all)
        ├── des_grid (gen_server) — grid state via ETS
        ├── des_scheduler (gen_server) — event-driven simulation engine
        └── des_bot_sup (simple_one_for_one supervisor)
              └── des_bot (gen_server) × N — one process per bot
```

### Simulation Flow

True event-driven DES (time jumps between events, not fixed increments):

1. Bots spawn at random positions, compute A* path to random destination
2. Each bot schedules its first move event at T=0
3. Scheduler pops smallest-time event from gb_trees priority queue
4. Event dispatched synchronously to target bot's gen_server
5. Bot moves one step, schedules next event at T+1 (or replans if path exhausted/destination reached)
6. Loop until simulation end time exceeded

## Key Modules

| Module | File | Purpose |
|--------|------|---------|
| `des_sim_app` | `src/des_sim_app.erl` | OTP application entry point, config loading, bot spawning |
| `des_sim_sup` | `src/des_sim_sup.erl` | Main supervisor (one_for_all strategy) |
| `des_scheduler` | `src/des_scheduler.erl` | Core DES engine — priority queue, event dispatch, stats |
| `des_grid` | `src/des_grid.erl` | Grid state management via ETS table `des_bot_positions` |
| `des_bot` | `src/des_bot.erl` | Individual bot agent — movement, pathfinding, analytics |
| `des_bot_sup` | `src/des_bot_sup.erl` | Dynamic bot supervisor (simple_one_for_one) |
| `des_astar` | `src/des_astar.erl` | A* pathfinding (Manhattan heuristic, 4-directional) |
| `cowhca_util` | `src/cowhca_util.erl` | Pathfinding abstraction — local A* or remote COWHCA via RPC |
| `des_pq_demo` | `src/des_pq_demo.erl` | Priority queue demonstration/visualization |

## Configuration

Configured via `config/sys.config` (defaults) or `config/sys.local.config` (local overrides, gitignored).

| Parameter | Default | Description |
|-----------|---------|-------------|
| `grid_width` | 50 | Grid width in cells |
| `grid_height` | 50 | Grid height in cells |
| `num_bots` | 10 | Number of bot processes |
| `bot_speed` | 1 | Units per simulation second |
| `simulation_duration_hrs` | 1 | Simulation length in hours |
| `pathfinder` | `local` | `local` (des_astar) or `cowhca` (remote RPC) |
| `cowhca_node` | `butler_server@localhost` | COWHCA node name |
| `cowhca_cookie` | `butler_server` | COWHCA auth cookie |

## Code Conventions

- OTP behaviors: `gen_server`, `supervisor`, `application`
- Registered processes: `des_scheduler`, `des_grid`, `des_bot_sup`, `des_sim_sup`
- Bot processes are temporary (no restart on crash)
- ETS table `des_bot_positions` has public read access for concurrent reads
- Priority queue keys are `{Time, Counter}` tuples for uniqueness at same timestamp
