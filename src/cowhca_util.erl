%%%-------------------------------------------------------------------
%%% @doc COWHCA Utility - Config-based pathfinding via butler_server RPC
%%%
%%% Config options (in sys.config or application env):
%%%   {pathfinder, local | cowhca}  - which pathfinder to use
%%%   {cowhca_node, 'butler_server@localhost'}
%%%   {cowhca_cookie, 'butler_server'}
%%% @end
%%%-------------------------------------------------------------------
-module(cowhca_util).

-export([
    find_path/2,
    find_path/3,
    find_path/4,
    find_path_for_bot/3,
    find_path_cowhca/3,
    find_path_local/4,
    extract_coordinates/1,
    test_path/0,
    test_path/2,
    connect/0,
    connect/1,
    connect/2,
    benchmark/0,
    benchmark/1,
    benchmark/3,
    update_moving_reservation/4,
    remove_moving_reservation/3,
    bot_id_to_integer/1
]).

%% Defaults
-define(DEFAULT_NODE, 'butler_server@localhost').
-define(DEFAULT_COOKIE, 'butler_server').
-define(DEFAULT_PATHFINDER, local).  %% local | cowhca

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Find path based on config (reads from application env)
%% Returns {ok, Path} where Path is list of {X, Y} coordinates
%% Uses default butler ID (1) for COWHCA
find_path(Start, Goal) ->
    Config = get_config(),
    find_path(Start, Goal, Config).

%% @doc Find path with explicit config
%% Config is a map with keys: pathfinder, grid_width, grid_height, cowhca_node, cowhca_cookie
%% Uses default butler ID (1) for COWHCA
find_path(Start, Goal, Config) ->
    find_path(Start, Goal, Config, 1).

%% @doc Find path with explicit config and butler ID
%% ButlerId can be atom (bot_1) or integer (1) - converted to integer for butler_server
find_path(Start, Goal, Config, ButlerId) ->
    Pathfinder = maps:get(pathfinder, Config, ?DEFAULT_PATHFINDER),
    case Pathfinder of
        cowhca ->
            find_path_cowhca(Start, Goal, ButlerId);
        local ->
            GridWidth = maps:get(grid_width, Config, 50),
            GridHeight = maps:get(grid_height, Config, 50),
            find_path_local(Start, Goal, GridWidth, GridHeight);
        _ ->
            {error, {unknown_pathfinder, Pathfinder}}
    end.

%% @doc Find path with butler ID using default config
%% Convenience function for des_bot to pass bot ID without needing explicit config
%% ButlerId can be atom (bot_1) or integer (1) - converted to integer for butler_server
find_path_for_bot(Start, Goal, ButlerId) ->
    Config = get_config(),
    find_path(Start, Goal, Config, ButlerId).

%% @doc Find path using local A* algorithm
find_path_local(Start, Goal, GridWidth, GridHeight) ->
    des_astar:find_path(Start, Goal, GridWidth, GridHeight).

%% @doc Find path using butler_server COWHCA via RPC
%% Returns {ok, Path} where Path is list of {X, Y} coordinates
%% ButlerId can be atom (bot_1) or integer (1) - converted to integer for butler_server
find_path_cowhca(Start, Goal, ButlerId) ->
    ensure_distribution_started(),
    Node = get_cowhca_node(),
    Cookie = get_cowhca_cookie(),
    erlang:set_cookie(Node, Cookie),
    IntButlerId = bot_id_to_integer(ButlerId),
    case call_path_calc(Node, Start, Goal, IntButlerId) of
        {ok, {no_path, Reason}} ->
            {error, {no_path, Reason}};
        {ok, RawResult} ->
            Path = extract_coordinates(RawResult),
            case Path of
                [] -> {error, no_path};
                _ -> {ok, Path}
            end;
        Error ->
            Error
    end.

%% @doc Book a moving reservation on butler_server for a coordinate
%% ResStartTime and ResEndTime are in Unix epoch milliseconds
%% ButlerId can be atom (bot_1) or integer (1) - converted to integer for butler_server
-spec update_moving_reservation(atom() | integer(), {integer(), integer()}, integer(), integer()) ->
    ok | {error, term()}.
update_moving_reservation(ButlerId, Coordinate, ResStartTime, ResEndTime) ->
    ensure_distribution_started(),
    Node = get_cowhca_node(),
    Cookie = get_cowhca_cookie(),
    erlang:set_cookie(Node, Cookie),
    IntButlerId = bot_id_to_integer(ButlerId),
    Msg = {update_moving_reservation, IntButlerId, Coordinate, ResStartTime, ResEndTime},
    try
        gen_server:call({mapf_path_calculator, Node}, Msg, 5000)
    catch
        exit:{noproc, _} -> {error, noproc};
        exit:{timeout, _} -> {error, timeout};
        _:Reason -> {error, Reason}
    end.

%% @doc Remove/clear a moving reservation on butler_server after bot has moved
%% Bdir is the direction (north, south, east, west)
%% ButlerId can be atom (bot_1) or integer (1) - converted to integer for butler_server
-spec remove_moving_reservation(atom() | integer(), {integer(), integer()}, atom()) ->
    ok | {error, term()}.
remove_moving_reservation(ButlerId, Coordinate, Bdir) ->
    ensure_distribution_started(),
    Node = get_cowhca_node(),
    Cookie = get_cowhca_cookie(),
    erlang:set_cookie(Node, Cookie),
    IntButlerId = bot_id_to_integer(ButlerId),
    Msg = {remove_moving_reservation, IntButlerId, Coordinate, Bdir},
    try
        gen_server:call({mapf_path_calculator, Node}, Msg, 5000)
    catch
        exit:{noproc, _} -> {error, noproc};
        exit:{timeout, _} -> {error, timeout};
        _:Reason -> {error, Reason}
    end.

%% @doc Extract coordinates from COWHCA result
%% Input: {goal, [#coordinate_movement_info{}, ...]} or list of coordinate_movement_info tuples
%% Output: [{X, Y}, ...]
extract_coordinates({goal, CoordInfoList}) ->
    extract_coordinates(CoordInfoList);
extract_coordinates({window, CoordInfoList}) ->
    extract_coordinates(CoordInfoList);
extract_coordinates({no_path, _}) ->
    [];
extract_coordinates(CoordInfoList) when is_list(CoordInfoList) ->
    [extract_coord(Info) || Info <- CoordInfoList];
extract_coordinates(_) ->
    [].

%% @doc Connect to butler_server node
connect() ->
    connect(get_cowhca_node(), get_cowhca_cookie()).

connect(Node) ->
    connect(Node, get_cowhca_cookie()).

connect(Node, Cookie) ->
    %% Ensure distribution is started
    ensure_distribution_started(),
    erlang:set_cookie(Node, Cookie),
    case net_adm:ping(Node) of
        pong ->
            io:format("Connected to ~p~n", [Node]),
            {ok, Node};
        pang ->
            io:format("Failed to connect to ~p~n", [Node]),
            io:format("Make sure both nodes use the same cookie.~n"),
            io:format("Registered nodes: ~p~n", [net_adm:names()]),
            {error, cannot_connect}
    end.

%% Start distribution if not already started
ensure_distribution_started() ->
    case node() of
        'nonode@nohost' ->
            %% Not distributed, start it
            NodeName = list_to_atom("des_sim_" ++ integer_to_list(erlang:system_time(millisecond))),
            case net_kernel:start([NodeName, shortnames]) of
                {ok, _Pid} ->
                    io:format("Started distribution as ~p~n", [node()]),
                    ok;
                {error, Reason} ->
                    io:format("Warning: Could not start distribution: ~p~n", [Reason]),
                    {error, Reason}
            end;
        _ ->
            %% Already distributed
            ok
    end.

%% @doc Test path calculation with hardcoded values
test_path() ->
    test_path({25, 25}, {25, 30}).

%% @doc Test path calculation from Start to Goal using COWHCA
test_path(Start, Goal) ->
    case connect() of
        {ok, _Node} ->
            Result = find_path_cowhca(Start, Goal, 1),
            io:format("Path coordinates: ~p~n", [Result]),
            Result;
        Error ->
            Error
    end.

%% @doc Benchmark RPC call latency with default 100 iterations
benchmark() ->
    benchmark(100).

%% @doc Benchmark RPC call latency with N iterations
benchmark(N) ->
    benchmark(N, {25, 25}, {25, 30}).

%% @doc Benchmark RPC call latency with N iterations from Start to Goal
benchmark(N, Start, Goal) ->
    case connect() of
        {ok, _Node} ->
            io:format("~nBenchmarking ~p RPC calls from ~p to ~p...~n", [N, Start, Goal]),

            %% Warmup - 5 calls
            io:format("Warmup (5 calls)...~n"),
            [find_path_cowhca(Start, Goal, 1) || _ <- lists:seq(1, 5)],

            %% Run benchmark
            Times = [begin
                T1 = erlang:monotonic_time(microsecond),
                _Result = find_path_cowhca(Start, Goal, 1),
                T2 = erlang:monotonic_time(microsecond),
                T2 - T1
            end || _ <- lists:seq(1, N)],

            %% Calculate statistics
            SortedTimes = lists:sort(Times),
            Min = hd(SortedTimes),
            Max = lists:last(SortedTimes),
            Sum = lists:sum(Times),
            Avg = Sum / N,
            Median = lists:nth(N div 2, SortedTimes),
            P95 = lists:nth(round(N * 0.95), SortedTimes),
            P99 = lists:nth(round(N * 0.99), SortedTimes),

            io:format("~n========== Benchmark Results ==========~n"),
            io:format("  Iterations: ~p~n", [N]),
            io:format("  Min:        ~.2f ms~n", [Min / 1000]),
            io:format("  Max:        ~.2f ms~n", [Max / 1000]),
            io:format("  Avg:        ~.2f ms~n", [Avg / 1000]),
            io:format("  Median:     ~.2f ms~n", [Median / 1000]),
            io:format("  P95:        ~.2f ms~n", [P95 / 1000]),
            io:format("  P99:        ~.2f ms~n", [P99 / 1000]),
            io:format("  Total:      ~.2f ms~n", [Sum / 1000]),
            io:format("========================================~n"),

            #{
                iterations => N,
                min_us => Min,
                max_us => Max,
                avg_us => Avg,
                median_us => Median,
                p95_us => P95,
                p99_us => P99,
                total_us => Sum
            };
        Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Extract coordinate from coordinate_movement_info tuple
%% The record is: {coordinate_movement_info, Coordinate, ...}
extract_coord(Info) when is_tuple(Info) ->
    case element(1, Info) of
        coordinate_movement_info ->
            element(2, Info);  %% coordinate is the 2nd element
        _ ->
            Info
    end;
extract_coord(Coord) ->
    Coord.

call_path_calc(Node, {StartX, StartY}, {GoalX, GoalY}, ButlerId) ->
    Timestamp = erlang:monotonic_time(millisecond),
    Result = rpc:call(Node, co_whca_test, path_calc_test, [
        ButlerId,               % Arg 1: butler id (integer)
        undefined,              % Arg 2
        butler@v2_1,            % Arg 3: butler type
        lift_down,              % Arg 4: action
        #{},                    % Arg 5: reservation table
        {StartX, StartY},       % Arg 6: start position
        north,                  % Arg 7: start direction
        undefined,              % Arg 8
        {GoalX, GoalY},         % Arg 9: goal position
        north,                  % Arg 10: goal direction
        Timestamp               % Arg 11: timestamp
    ], 5000),
    case Result of
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}};
        PathResult ->
            {ok, PathResult}
    end.

%% Get config from application env
get_config() ->
    #{
        pathfinder => get_env(pathfinder, ?DEFAULT_PATHFINDER),
        grid_width => get_env(grid_width, 50),
        grid_height => get_env(grid_height, 50),
        cowhca_node => get_cowhca_node(),
        cowhca_cookie => get_cowhca_cookie()
    }.

get_cowhca_node() ->
    get_env(cowhca_node, ?DEFAULT_NODE).

get_cowhca_cookie() ->
    get_env(cowhca_cookie, ?DEFAULT_COOKIE).

get_env(Key, Default) ->
    case application:get_env(des_sim, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

%% @doc Convert bot atom id (bot_1, bot_2, etc.) to integer (1, 2, etc.)
%% for butler_server communication
-spec bot_id_to_integer(atom()) -> integer().
bot_id_to_integer(BotId) when is_atom(BotId) ->
    BotIdStr = atom_to_list(BotId),
    case string:prefix(BotIdStr, "bot_") of
        nomatch ->
            %% Try to parse as integer directly
            case catch list_to_integer(BotIdStr) of
                N when is_integer(N) -> N;
                _ -> 1  %% Default to 1 if unparseable
            end;
        NumStr ->
            list_to_integer(NumStr)
    end;
bot_id_to_integer(BotId) when is_integer(BotId) ->
    BotId.
