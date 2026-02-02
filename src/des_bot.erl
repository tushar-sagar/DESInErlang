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
    last_event_time :: float()                 % Last event time
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
    %% Get grid config
    {GridWidth, GridHeight} = des_grid:get_config(),

    %% Generate random start position and destination
    StartPos = des_grid:random_position(),
    Destination = generate_different_position(StartPos, GridWidth, GridHeight),

    %% Compute initial path with timing
    {PathTime, {ok, Path}} = timer:tc(fun() ->
        des_astar:find_path(StartPos, Destination, GridWidth, GridHeight)
    end),
    PathLength = length(Path) - 1,  % Exclude start position

    %% Update grid with initial position
    des_grid:update_bot_position(BotId, StartPos, Destination),

    %% Log spawn
    io:format("[T=0.000] ~p spawned at ~p, target ~p (path_len=~p)~n",
              [BotId, StartPos, Destination, PathLength]),

    %% Schedule first move event at T=0
    des_scheduler:schedule_event(0.0, bot_init, #{bot_id => BotId}),

    {ok, #state{
        id = BotId,
        position = StartPos,
        destination = Destination,
        path = Path,
        grid_width = GridWidth,
        grid_height = GridHeight,
        trips_completed = 0,
        total_moves = 0,
        bot_speed = 1,
        %% Initialize analytics
        path_requests = 1,
        total_path_length = PathLength,
        path_compute_time_us = PathTime,
        total_distance = 0,
        start_time = 0.0,
        last_event_time = 0.0
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
        active_time => State#state.last_event_time - State#state.start_time
    },
    {reply, Stats, State};

%% Handle events from the DES scheduler (synchronous)
handle_call({event, _EventType, SimTime, _Data}, _From, State) ->
    NewState = do_move(SimTime, State#state{last_event_time = SimTime}),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_move(SimTime, #state{path = [_Current | []]} = State) ->
    %% Reached destination (path only contains current position)
    #state{
        id = BotId,
        position = Pos,
        destination = Dest,
        grid_width = W,
        grid_height = H,
        trips_completed = Trips,
        bot_speed = Speed,
        path_requests = PathReqs,
        total_path_length = TotalPathLen,
        path_compute_time_us = TotalPathTime
    } = State,

    %% Generate new destination
    NewDest = generate_different_position(Pos, W, H),

    %% Compute path with timing
    {PathTime, {ok, NewPath}} = timer:tc(fun() ->
        des_astar:find_path(Pos, NewDest, W, H)
    end),
    PathLength = length(NewPath) - 1,

    %% Update grid
    des_grid:update_bot_position(BotId, Pos, NewDest),

    %% Log arrival and new destination
    io:format("[T=~.3f] ~p reached ~p, new target ~p (path_len=~p)~n",
              [SimTime, BotId, Dest, NewDest, PathLength]),

    %% Schedule next move event
    NextTime = SimTime + (1.0 / Speed),
    des_scheduler:schedule_event(NextTime, bot_move, #{}),

    State#state{
        destination = NewDest,
        path = NewPath,
        trips_completed = Trips + 1,
        path_requests = PathReqs + 1,
        total_path_length = TotalPathLen + PathLength,
        path_compute_time_us = TotalPathTime + PathTime
    };

do_move(SimTime, #state{path = [_Current, NextPos | RestPath]} = State) ->
    %% Move to next position in path
    #state{
        id = BotId,
        position = OldPos,
        destination = Dest,
        total_moves = Moves,
        total_distance = Distance,
        bot_speed = Speed
    } = State,

    %% Update grid
    des_grid:update_bot_position(BotId, NextPos, Dest),

    %% Calculate step distance (always 1 for 4-directional)
    StepDist = manhattan_distance(OldPos, NextPos),

    %% Schedule next move event
    NextTime = SimTime + (1.0 / Speed),
    des_scheduler:schedule_event(NextTime, bot_move, #{}),

    State#state{
        position = NextPos,
        path = [NextPos | RestPath],
        total_moves = Moves + 1,
        total_distance = Distance + StepDist
    };

do_move(SimTime, #state{bot_speed = Speed} = State) ->
    %% Empty path - shouldn't happen, but schedule next event anyway
    NextTime = SimTime + (1.0 / Speed),
    des_scheduler:schedule_event(NextTime, bot_move, #{}),
    State.

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
