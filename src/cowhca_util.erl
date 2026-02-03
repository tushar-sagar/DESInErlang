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
    find_path_cowhca/2,
    find_path_local/4,
    extract_coordinates/1,
    test_path/0,
    test_path/2,
    connect/0,
    connect/1,
    connect/2,
    benchmark/0,
    benchmark/1,
    benchmark/3
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
find_path(Start, Goal) ->
    Config = get_config(),
    find_path(Start, Goal, Config).

%% @doc Find path with explicit config
%% Config is a map with keys: pathfinder, grid_width, grid_height, cowhca_node, cowhca_cookie
find_path(Start, Goal, Config) ->
    Pathfinder = maps:get(pathfinder, Config, ?DEFAULT_PATHFINDER),
    case Pathfinder of
        cowhca ->
            find_path_cowhca(Start, Goal);
        local ->
            GridWidth = maps:get(grid_width, Config, 50),
            GridHeight = maps:get(grid_height, Config, 50),
            find_path_local(Start, Goal, GridWidth, GridHeight);
        _ ->
            {error, {unknown_pathfinder, Pathfinder}}
    end.

%% @doc Find path using local A* algorithm
find_path_local(Start, Goal, GridWidth, GridHeight) ->
    des_astar:find_path(Start, Goal, GridWidth, GridHeight).

%% @doc Find path using butler_server COWHCA via RPC
%% Returns {ok, Path} where Path is list of {X, Y} coordinates
find_path_cowhca(Start, Goal) ->
    ensure_distribution_started(),
    Node = get_cowhca_node(),
    Cookie = get_cowhca_cookie(),
    erlang:set_cookie(Node, Cookie),
    case call_path_calc(Node, Start, Goal) of
        {ok, RawResult} ->
            Path = extract_coordinates(RawResult),
            {ok, Path};
        Error ->
            Error
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
            Result = find_path_cowhca(Start, Goal),
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
            [find_path_cowhca(Start, Goal) || _ <- lists:seq(1, 5)],

            %% Run benchmark
            Times = [begin
                T1 = erlang:monotonic_time(microsecond),
                _Result = find_path_cowhca(Start, Goal),
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

call_path_calc(Node, {StartX, StartY}, {GoalX, GoalY}) ->
    Timestamp = erlang:monotonic_time(millisecond),
    Result = rpc:call(Node, co_whca_test, path_calc_test, [
        1,                      % Arg 1: butler id
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
    % io:format("Result from COWHCA RPC: ~p~n", [Result]),
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
