# Reservation Events Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `book_reservation` as a DES event that calls butler_server's `update_moving_reservation` RPC before each bot move, using synthetic monotonic timestamps mapped from simulation time.

**Architecture:** The DES scheduler dispatches all events synchronously. A new `book_reservation` event precedes each `do_move`. Bots compute synthetic monotonic timestamps from a shared base captured at simulation start. On reservation conflict, the bot cancels remaining bookings and replans via COWHCA.

**Tech Stack:** Erlang/OTP, gb_trees priority queue, gen_server:call for cross-node reservation RPC

---

### Task 1: Add `update_moving_reservation` to cowhca_util

**Files:**
- Modify: `src/cowhca_util.erl`

**Step 1: Add export and function**

Add `update_moving_reservation/4` to the export list and implement the function. This calls butler_server's `mapf_path_calculator` registered process directly via distributed gen_server:call.

```erlang
%% Add to export list:
update_moving_reservation/4

%% Add function:
%% @doc Book a moving reservation on butler_server for a coordinate
%% ResStartTime and ResEndTime are in erlang monotonic milliseconds
-spec update_moving_reservation(atom(), {integer(), integer()}, integer(), integer()) ->
    ok | {error, term()}.
update_moving_reservation(ButlerId, Coordinate, ResStartTime, ResEndTime) ->
    ensure_distribution_started(),
    Node = get_cowhca_node(),
    Cookie = get_cowhca_cookie(),
    erlang:set_cookie(Node, Cookie),
    Msg = {update_moving_reservation, ButlerId, Coordinate, ResStartTime, ResEndTime},
    try
        gen_server:call({mapf_path_calculator, Node}, Msg, 5000)
    catch
        exit:{noproc, _} -> {error, noproc};
        exit:{timeout, _} -> {error, timeout};
        _:Reason -> {error, Reason}
    end.
```

**Step 2: Compile and verify**

Run: `rebar3 compile`
Expected: SUCCESS (no callers yet, just verify it compiles)

**Step 3: Commit**

```bash
git add src/cowhca_util.erl
git commit -m "feat: add update_moving_reservation RPC to cowhca_util"
```

---

### Task 2: Modify des_scheduler for sync dispatch and base_monotonic

**Files:**
- Modify: `src/des_scheduler.erl`

**Step 1: Add `base_monotonic` to state record and export**

Add `base_monotonic` field to `#state{}` record. Add `get_base_monotonic/0` API function.

In the state record, add after `start_wall_time`:
```erlang
base_monotonic :: integer(),  % Monotonic time at simulation start (for reservation mapping)
```

Add to exports:
```erlang
get_base_monotonic/0
```

Add API function:
```erlang
get_base_monotonic() ->
    gen_server:call(?SERVER, get_base_monotonic).
```

Add handle_call clause:
```erlang
handle_call(get_base_monotonic, _From, State) ->
    {reply, State#state.base_monotonic, State};
```

Initialize in `init/1`:
```erlang
base_monotonic = 0
```

Set in `handle_cast(start_simulation, ...)`:
```erlang
WallStart = erlang:monotonic_time(millisecond),
NewState = State#state{
    running = true,
    start_wall_time = WallStart,
    base_monotonic = WallStart
},
```

**Step 2: Make all events dispatch synchronously**

Replace the `process_event/2` function. Remove the special-case async `do_move` clause. All events go through sync `gen_server:call`:

```erlang
process_event(#event{type = Type, pid = Pid, data = Data}, State) ->
    try
        gen_server:call(Pid, {event, Type, State#state.current_time, Data}, infinity)
    catch
        exit:{noproc, _} -> ok;
        exit:{timeout, _} -> ok
    end,
    State.
```

**Step 3: Compile and verify**

Run: `rebar3 compile`
Expected: SUCCESS

**Step 4: Commit**

```bash
git add src/des_scheduler.erl
git commit -m "feat: add base_monotonic to scheduler, make all events sync"
```

---

### Task 3: Modify des_bot for reservation events

**Files:**
- Modify: `src/des_bot.erl`

This is the largest change. The bot needs:
- New state fields: `base_monotonic`, `reservation_failures`
- `book_reservation` event handler
- `do_move` changed from cast to call handler
- `schedule_path_events` emits `book_reservation` instead of `do_move`

**Step 1: Update state record**

Add fields after `last_event_time`:
```erlang
base_monotonic :: integer(),            % Shared monotonic base for reservation timestamps
reservation_failures :: non_neg_integer() % Count of reservation conflicts
```

Initialize in `init/1`:
```erlang
base_monotonic = 0,
reservation_failures = 0
```

**Step 2: Fetch base_monotonic at first plan_path event**

Modify the `handle_call({event, plan_path, ...})` clause to lazily fetch `base_monotonic` from scheduler on first call (when it's still 0):

```erlang
handle_call({event, plan_path, SimTime, _Data}, _From, State) ->
    State1 = maybe_fetch_base_monotonic(State),
    NewState = do_plan_path(SimTime, State1#state{last_event_time = SimTime}),
    {reply, ok, NewState};
```

Add helper:
```erlang
maybe_fetch_base_monotonic(#state{base_monotonic = 0} = State) ->
    Base = des_scheduler:get_base_monotonic(),
    State#state{base_monotonic = Base};
maybe_fetch_base_monotonic(State) ->
    State.
```

**Step 3: Add `book_reservation` event handler**

Add a new `handle_call` clause before the catch-all:

```erlang
%% book_reservation: synchronous — book on butler_server before move
handle_call({event, book_reservation, SimTime, Data}, _From, State) ->
    #{pos := NextPos, step_idx := StepIdx} = Data,
    #state{
        id = BotId,
        bot_speed = Speed,
        base_monotonic = Base
    } = State,

    ResStartTime = Base + round(SimTime * 1000),
    ResEndTime = ResStartTime + round(1000 / Speed),

    case cowhca_util:update_moving_reservation(BotId, NextPos, ResStartTime, ResEndTime) of
        ok ->
            %% Reservation succeeded, schedule do_move at same sim time
            des_scheduler:schedule_event(SimTime, do_move, #{pos => NextPos}),
            {reply, ok, State#state{last_event_time = SimTime}};
        {error, _Reason} ->
            %% Reservation conflict — schedule replan
            io:format("[T=~.3f] ~p reservation conflict at ~p, replanning~n",
                      [SimTime, BotId, NextPos]),
            des_scheduler:schedule_event(SimTime, plan_path, #{}),
            {reply, ok, State#state{
                last_event_time = SimTime,
                reservation_failures = State#state.reservation_failures + 1
            }}
    end;
```

**Step 4: Change do_move from cast to call handler**

Remove the existing `handle_cast({event, do_move, ...})` clause. Add a `handle_call` clause:

```erlang
%% do_move: synchronous — move bot on grid
handle_call({event, do_move, SimTime, Data}, _From, State) ->
    #{pos := NextPos} = Data,
    #state{
        id = BotId,
        position = OldPos,
        destination = Dest,
        total_moves = Moves,
        total_distance = Distance
    } = State,

    des_grid:update_bot_position(BotId, NextPos, Dest),
    StepDist = manhattan_distance(OldPos, NextPos),

    {reply, ok, State#state{
        position = NextPos,
        total_moves = Moves + 1,
        total_distance = Distance + StepDist,
        last_event_time = SimTime
    }};
```

**Step 5: Update schedule_path_events to emit book_reservation**

Replace `schedule_steps`:

```erlang
schedule_steps([], SimTime, Speed, StepIdx) ->
    %% All reservations scheduled, schedule next planning event
    PlanTime = SimTime + (StepIdx * 1.0 / Speed),
    des_scheduler:schedule_event(PlanTime, plan_path, #{});
schedule_steps([Pos | Rest], SimTime, Speed, StepIdx) ->
    Time = SimTime + (StepIdx * 1.0 / Speed),
    des_scheduler:schedule_event(Time, book_reservation, #{pos => Pos, step_idx => StepIdx}),
    schedule_steps(Rest, SimTime, Speed, StepIdx + 1).
```

**Step 6: Add reservation_failures to stats**

In `handle_call(get_stats, ...)`, add to the Stats map:

```erlang
reservation_failures => State#state.reservation_failures
```

In `finish_simulation` in des_scheduler.erl, add aggregate reservation failure count to the report (optional, can be done later).

**Step 7: Compile and verify**

Run: `rebar3 compile`
Expected: SUCCESS

**Step 8: Commit**

```bash
git add src/des_bot.erl
git commit -m "feat: add book_reservation event, sync do_move, synthetic monotonic timestamps"
```

---

### Task 4: Test with local pathfinder

**Step 1: Run simulation with local A***

Since local A* has no actual butler_server, `update_moving_reservation` will fail with `{error, noproc}`. This means every reservation will trigger a replan — infinite loop.

We need to handle the `pathfinder = local` case: skip reservation when not using COWHCA.

Modify `book_reservation` handler in des_bot to check pathfinder config:

```erlang
handle_call({event, book_reservation, SimTime, Data}, _From, State) ->
    #{pos := NextPos, step_idx := _StepIdx} = Data,
    #state{
        id = BotId,
        bot_speed = Speed,
        base_monotonic = Base
    } = State,

    Pathfinder = application:get_env(des_sim, pathfinder, local),
    BookResult = case Pathfinder of
        local ->
            %% No butler_server, skip reservation
            ok;
        cowhca ->
            ResStartTime = Base + round(SimTime * 1000),
            ResEndTime = ResStartTime + round(1000 / Speed),
            cowhca_util:update_moving_reservation(BotId, NextPos, ResStartTime, ResEndTime)
    end,

    case BookResult of
        ok ->
            des_scheduler:schedule_event(SimTime, do_move, #{pos => NextPos}),
            {reply, ok, State#state{last_event_time = SimTime}};
        {error, _Reason} ->
            io:format("[T=~.3f] ~p reservation conflict at ~p, replanning~n",
                      [SimTime, BotId, NextPos]),
            des_scheduler:schedule_event(SimTime, plan_path, #{}),
            {reply, ok, State#state{
                last_event_time = SimTime,
                reservation_failures = State#state.reservation_failures + 1
            }}
    end;
```

**Step 2: Run simulation**

```bash
rebar3 shell
des_sim_app:run(#{num_bots => 3, simulation_duration_hrs => 0.01}).
```

Expected: Simulation runs to completion with local A*. Output should show bots moving, planning, completing trips. The `book_reservation` events pass through as no-ops in local mode.

**Step 3: Commit**

```bash
git add src/des_bot.erl
git commit -m "feat: skip reservation booking when pathfinder is local"
```

---

### Task 5: Update simulation report with reservation stats

**Files:**
- Modify: `src/des_scheduler.erl` (finish_simulation)
- Modify: `src/des_bot.erl` (get_stats)

**Step 1: Add reservation_failures to bot stats map**

Already done in Task 3 Step 6. Verify it's present.

**Step 2: Add reservation stats to finish_simulation report**

In `des_scheduler:finish_simulation/1`, after the aggregate stats block, add:

```erlang
TotalResFails = lists:sum([maps:get(reservation_failures, S, 0) || S <- AllStats]),
```

And print it:
```erlang
io:format("  Reservation conflicts: ~p~n", [TotalResFails]),
```

**Step 3: Compile and run**

```bash
rebar3 compile
rebar3 shell
des_sim_app:run(#{num_bots => 3, simulation_duration_hrs => 0.01}).
```

Expected: Report now includes "Reservation conflicts: 0" (since local mode skips reservations).

**Step 4: Commit**

```bash
git add src/des_scheduler.erl src/des_bot.erl
git commit -m "feat: add reservation conflict stats to simulation report"
```
