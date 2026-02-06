%%%-------------------------------------------------------------------
%%% @doc Bot process - represents a single bot on the grid
%%% Each bot schedules its own events with the DES scheduler
%%% Tracks detailed analytics for simulation analysis
%%% @end
%%%-------------------------------------------------------------------
-module(des_bot).
-behaviour(gen_server).

%% API
-export([start_link/1,
         get_state/1,
         get_stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    id :: atom(),
    position :: {integer(), integer()},
    direction :: north | south | east | west,  % Current facing direction
    destination :: {integer(), integer()},
    path :: [{integer(), integer()}],
    grid_width :: pos_integer(),
    grid_height :: pos_integer(),
    trips_completed :: non_neg_integer(),
    total_moves :: non_neg_integer(),
    bot_speed :: pos_integer(),
    %% Analytics
    path_requests :: non_neg_integer(),        % Number of A* calls
    total_path_length :: non_neg_integer(),    % Sum of all path lengths
    path_compute_time_us :: non_neg_integer(), % Total A* computation time (microseconds)
    total_distance :: non_neg_integer(),       % Total cells traveled
    start_time :: float(),                     % First event time
    last_event_time :: float(),                % Last event time
    %% Reservation support
    base_monotonic :: integer(),               % System time (Unix epoch ms) base for reservation timestamps
    reservation_failures :: non_neg_integer()  % Count of reservation conflicts
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(BotId) ->
    gen_server:start_link({local, BotId}, ?MODULE, BotId, []).

%% @doc Get bot state
get_state(BotPid) ->
    gen_server:call(BotPid, get_state, infinity).

%% @doc Get bot statistics
get_stats(BotPid) ->
    gen_server:call(BotPid, get_stats, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(BotId) ->
    {GridWidth, GridHeight} = des_grid:get_config(),

    StartPos = des_grid:random_position(),
    Destination = generate_different_position(StartPos, GridWidth, GridHeight),

    des_grid:update_bot_position(BotId, StartPos, Destination),

    io:format("[T=0.000] ~p spawned at ~p, target ~p~n",
              [BotId, StartPos, Destination]),

    %% Schedule initial planning event at T=0
    des_scheduler:schedule_event(0.0, plan_path, #{bot_id => BotId}),

    {ok, #state{
        id = BotId,
        position = StartPos,
        direction = north,  % Default facing direction
        destination = Destination,
        path = [],
        grid_width = GridWidth,
        grid_height = GridHeight,
        trips_completed = 0,
        total_moves = 0,
        bot_speed = 1,
        path_requests = 0,
        total_path_length = 0,
        path_compute_time_us = 0,
        total_distance = 0,
        start_time = 0.0,
        last_event_time = 0.0,
        base_monotonic = 0,
        reservation_failures = 0
    }}.

handle_call(get_state, _From, State) ->
    {reply, {State#state.position, State#state.destination, State#state.path}, State};

handle_call(get_stats, _From, State) ->
    Stats = #{
        id => State#state.id,
        trips_completed => State#state.trips_completed,
        total_moves => State#state.total_moves,
        current_position => State#state.position,
        current_destination => State#state.destination,
        %% Analytics
        path_requests => State#state.path_requests,
        total_path_length => State#state.total_path_length,
        path_compute_time_us => State#state.path_compute_time_us,
        total_distance => State#state.total_distance,
        avg_path_length => safe_div(State#state.total_path_length, State#state.path_requests),
        avg_path_compute_us => safe_div(State#state.path_compute_time_us, State#state.path_requests),
        active_time => State#state.last_event_time - State#state.start_time,
        reservation_failures => State#state.reservation_failures
    },
    {reply, Stats, State};

%% plan_path: synchronous — scheduler blocks until all events are scheduled
handle_call({event, plan_path, SimTime, Data}, _From, State) ->
    Base = maps:get(base_monotonic, Data, State#state.base_monotonic),
    NewState = do_plan_path(SimTime, State#state{last_event_time = SimTime, base_monotonic = Base}),
    {reply, ok, NewState};

%% book_reservation: synchronous — book on butler_server before move
handle_call({event, book_reservation, SimTime, Data}, _From, State) ->
    #{pos := NextPos} = Data,
    #state{
        id = BotId,
        bot_speed = Speed,
        base_monotonic = Base
    } = State,

    trace_log(BotId, "[T=~.3f] ~p book_reservation for ~p~n", [SimTime, BotId, NextPos]),

    Pathfinder = application:get_env(des_sim, pathfinder, local),
    BookResult = case Pathfinder of
        local ->
            %% No butler_server, skip reservation
            ok;
        cowhca ->
            ResStartTime = Base + round(SimTime * 1000),
            ResEndTime = ResStartTime + round(1000 / Speed),
            trace_log(BotId, "[T=~.3f] ~p requesting reservation at ~p from ~p to ~p~n",
                      [SimTime, BotId, NextPos, ResStartTime, ResEndTime]),
            cowhca_util:update_moving_reservation(BotId, NextPos, ResStartTime, ResEndTime)
    end,

    case BookResult of
        {error, _Reason} ->
            io:format("[T=~.3f] ~p reservation conflict at ~p, replanning~n",
                      [SimTime, BotId, NextPos]),
            des_scheduler:schedule_event(SimTime, plan_path, #{}),
            {reply, ok, State#state{
                last_event_time = SimTime,
                reservation_failures = State#state.reservation_failures + 1
            }};
        _Success ->
            %% ok, updated, or any non-error response from butler_server
            des_scheduler:schedule_event(SimTime, do_move, #{pos => NextPos}),
            {reply, ok, State#state{last_event_time = SimTime}}
    end;

%% do_move: synchronous — all events dispatched via gen_server:call
%% After moving, schedule next event from local path queue
handle_call({event, do_move, SimTime, Data}, _From, State) ->
    #{pos := NextPos} = Data,
    #state{
        id = BotId,
        position = OldPos,
        destination = Dest,
        path = Path,
        bot_speed = Speed,
        base_monotonic = Base,
        total_moves = Moves,
        total_distance = Distance
    } = State,

    %% Calculate new direction from movement
    NewDirection = calculate_direction(OldPos, NextPos),

    trace_log(BotId, "[T=~.3f] ~p do_move ~p -> ~p dir=~p (remaining=~p)~n",
              [SimTime, BotId, OldPos, NextPos, NewDirection, length(Path) - 1]),

    des_grid:update_bot_position(BotId, NextPos, Dest),
    StepDist = manhattan_distance(OldPos, NextPos),

    %% Pop completed step from path
    RemainingPath = tl(Path),

    %% Extend reservation window - book next step beyond current window
    WindowSize = application:get_env(des_sim, reservation_window, 3),
    extend_reservation_window(BotId, RemainingPath, SimTime, Speed, Base, WindowSize),

    %% Schedule next event
    NextTime = SimTime + (1.0 / Speed),
    case RemainingPath of
        [] ->
            %% Reached destination, schedule replanning
            trace_log(BotId, "[T=~.3f] ~p scheduling plan_path at T=~.3f~n",
                      [SimTime, BotId, NextTime]),
            des_scheduler:schedule_event(NextTime, plan_path, #{});
        [NextStep | _] ->
            %% More steps remaining, schedule next reservation
            trace_log(BotId, "[T=~.3f] ~p scheduling book_reservation for ~p at T=~.3f~n",
                      [SimTime, BotId, NextStep, NextTime]),
            des_scheduler:schedule_event(NextTime, book_reservation, #{pos => NextStep})
    end,

    {reply, ok, State#state{
        position = NextPos,
        direction = NewDirection,
        path = RemainingPath,
        total_moves = Moves + 1,
        total_distance = Distance + StepDist,
        last_event_time = SimTime
    }};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions - Plan Path
%%%===================================================================

do_plan_path(SimTime, #state{position = Pos, destination = Dest} = State)
  when Pos =:= Dest ->
    %% Reached destination - trip completed, plan new route
    #state{
        id = BotId,
        grid_width = W,
        grid_height = H,
        trips_completed = Trips,
        bot_speed = Speed,
        base_monotonic = Base,
        path_requests = PathReqs,
        total_path_length = TotalPathLen,
        path_compute_time_us = TotalPathTime
    } = State,

    NewDest = generate_different_position(Pos, W, H),

    {PathTime, PathResult} = timer:tc(fun() ->
        cowhca_util:find_path_for_bot(Pos, NewDest, BotId)
    end),

    case PathResult of
        {ok, NewPath} when length(NewPath) > 1 ->
            PathLength = length(NewPath) - 1,
            des_grid:update_bot_position(BotId, Pos, NewDest),

            io:format("[T=~.3f] ~p reached ~p, new target ~p (path_len=~p)~n",
                      [SimTime, BotId, Dest, NewDest, PathLength]),

            %% Store remaining path steps
            [_Current | Steps] = NewPath,

            %% Book initial reservation window (e.g., 3 steps ahead)
            WindowSize = application:get_env(des_sim, reservation_window, 3),
            book_reservation_window(BotId, Steps, SimTime, Speed, Base, WindowSize),

            %% Schedule first move
            [FirstStep | _] = Steps,
            NextTime = SimTime + (1.0 / Speed),
            des_scheduler:schedule_event(NextTime, book_reservation, #{pos => FirstStep}),

            State#state{
                destination = NewDest,
                path = Steps,
                trips_completed = Trips + 1,
                path_requests = PathReqs + 1,
                total_path_length = TotalPathLen + PathLength,
                path_compute_time_us = TotalPathTime + PathTime
            };
        {error, Reason} ->
            io:format("[T=~.3f] ~p no path from ~p to ~p: ~p, picking new destination...~n",
                      [SimTime, BotId, Pos, NewDest, Reason]),
            %% Pick a different destination and retry
            NextTime = SimTime + (1.0 / Speed),
            des_scheduler:schedule_event(NextTime, plan_path, #{}),
            State#state{
                destination = generate_different_position(Pos, W, H),
                path_requests = PathReqs + 1,
                path_compute_time_us = TotalPathTime + PathTime
            };
        _ ->
            io:format("[T=~.3f] ~p unexpected path result, retrying...~n", [SimTime, BotId]),
            NextTime = SimTime + (1.0 / Speed),
            des_scheduler:schedule_event(NextTime, plan_path, #{}),
            State#state{
                destination = generate_different_position(Pos, W, H),
                path_requests = PathReqs + 1,
                path_compute_time_us = TotalPathTime + PathTime
            }
    end;

do_plan_path(SimTime, State) ->
    %% Not at destination (initial plan or window/partial re-route)
    #state{
        id = BotId,
        position = Pos,
        destination = Dest,
        bot_speed = Speed,
        base_monotonic = Base,
        path_requests = PathReqs,
        total_path_length = TotalPathLen,
        path_compute_time_us = TotalPathTime
    } = State,

    {PathTime, PathResult} = timer:tc(fun() ->
        cowhca_util:find_path_for_bot(Pos, Dest, BotId)
    end),

    case PathResult of
        {ok, NewPath} when length(NewPath) > 1 ->
            PathLength = length(NewPath) - 1,
            des_grid:update_bot_position(BotId, Pos, Dest),

            io:format("[T=~.3f] ~p planning path from ~p to ~p (path_len=~p)~n",
                      [SimTime, BotId, Pos, Dest, PathLength]),

            %% Store remaining path steps
            [_Current | Steps] = NewPath,

            %% Book initial reservation window (e.g., 3 steps ahead)
            WindowSize = application:get_env(des_sim, reservation_window, 3),
            book_reservation_window(BotId, Steps, SimTime, Speed, Base, WindowSize),

            %% Schedule first move
            [FirstStep | _] = Steps,
            NextTime = SimTime + (1.0 / Speed),
            des_scheduler:schedule_event(NextTime, book_reservation, #{pos => FirstStep}),

            State#state{
                path = Steps,
                path_requests = PathReqs + 1,
                total_path_length = TotalPathLen + PathLength,
                path_compute_time_us = TotalPathTime + PathTime
            };
        {error, Reason} ->
            io:format("[T=~.3f] ~p no path from ~p to ~p: ~p, retrying...~n",
                      [SimTime, BotId, Pos, Dest, Reason]),
            %% Retry path planning after a short delay
            NextTime = SimTime + (1.0 / Speed),
            des_scheduler:schedule_event(NextTime, plan_path, #{}),
            State#state{path_requests = PathReqs + 1, path_compute_time_us = TotalPathTime + PathTime};
        _ ->
            io:format("[T=~.3f] ~p unexpected path result from ~p to ~p, retrying...~n",
                      [SimTime, BotId, Pos, Dest]),
            NextTime = SimTime + (1.0 / Speed),
            des_scheduler:schedule_event(NextTime, plan_path, #{}),
            State#state{path_requests = PathReqs + 1, path_compute_time_us = TotalPathTime + PathTime}
    end.

%%%===================================================================
%%% Internal functions - Helpers
%%%===================================================================

generate_different_position(CurrentPos, Width, Height) ->
    NewPos = {rand:uniform(Width) - 1, rand:uniform(Height) - 1},
    case NewPos of
        CurrentPos -> generate_different_position(CurrentPos, Width, Height);
        _ -> NewPos
    end.

manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

safe_div(_, 0) -> 0.0;
safe_div(A, B) -> A / B.

%% Trace logging - only logs if bot matches trace_bot config
trace_log(BotId, Format, Args) ->
    case application:get_env(des_sim, trace_bot, undefined) of
        BotId -> io:format(Format, Args);
        _ -> ok
    end.

%% Calculate direction from movement (OldPos -> NewPos)
calculate_direction({X1, Y1}, {X2, Y2}) ->
    DX = X2 - X1,
    DY = Y2 - Y1,
    if
        DX > 0 -> east;
        DX < 0 -> west;
        DY > 0 -> north;
        DY < 0 -> south;
        true -> north  % No movement, default to north
    end.

%% Book reservations for a window of steps (COWHCA mode only)
%% Steps = [Step1, Step2, ...], books reservations for first WindowSize steps
book_reservation_window(BotId, Steps, SimTime, Speed, Base, WindowSize) ->
    Pathfinder = application:get_env(des_sim, pathfinder, local),
    case Pathfinder of
        cowhca ->
            StepsToBook = lists:sublist(Steps, WindowSize),
            book_steps(BotId, StepsToBook, SimTime, Speed, Base, 1);
        local ->
            ok
    end.

%% Book reservation for each step in the list
book_steps(_BotId, [], _SimTime, _Speed, _Base, _StepIdx) ->
    ok;
book_steps(BotId, [Pos | Rest], SimTime, Speed, Base, StepIdx) ->
    StepTime = SimTime + (StepIdx * 1.0 / Speed),
    ResStartTime = Base + round(StepTime * 1000),
    ResEndTime = ResStartTime + round(1000 / Speed),
    trace_log(BotId, "[T=~.3f] ~p pre-booking step ~p: ~p from ~p to ~p~n",
              [SimTime, BotId, StepIdx, Pos, ResStartTime, ResEndTime]),
    cowhca_util:update_moving_reservation(BotId, Pos, ResStartTime, ResEndTime),
    book_steps(BotId, Rest, SimTime, Speed, Base, StepIdx + 1).

%% Book the next step beyond the current window to maintain window size
extend_reservation_window(BotId, Path, SimTime, Speed, Base, WindowSize) ->
    Pathfinder = application:get_env(des_sim, pathfinder, local),
    case Pathfinder of
        cowhca ->
            %% If there's a step at position WindowSize in the remaining path, book it
            case length(Path) >= WindowSize of
                true ->
                    NextStepToBook = lists:nth(WindowSize, Path),
                    StepTime = SimTime + (WindowSize * 1.0 / Speed),
                    ResStartTime = Base + round(StepTime * 1000),
                    ResEndTime = ResStartTime + round(1000 / Speed),
                    trace_log(BotId, "[T=~.3f] ~p extending window: booking ~p from ~p to ~p~n",
                              [SimTime, BotId, NextStepToBook, ResStartTime, ResEndTime]),
                    cowhca_util:update_moving_reservation(BotId, NextStepToBook, ResStartTime, ResEndTime);
                false ->
                    ok  % No more steps to pre-book
            end;
        local ->
            ok
    end.
