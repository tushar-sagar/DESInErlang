%%%-------------------------------------------------------------------
%%% @doc DES Simulation OTP Application
%%% @end
%%%-------------------------------------------------------------------
-module(des_sim_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([run/0, run/1]).

%% Default configuration
-define(DEFAULT_CONFIG, #{
    grid_width => 50,
    grid_height => 50,
    num_bots => 10,
    bot_speed => 1,
    simulation_duration_hrs => 1
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Run simulation with default config
run() ->
    run(?DEFAULT_CONFIG).

%% @doc Run simulation with custom config
run(Config) ->
    FullConfig = maps:merge(?DEFAULT_CONFIG, Config),
    application:set_env(des_sim, config, FullConfig),
    application:ensure_all_started(des_sim).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    Config = application:get_env(des_sim, config, ?DEFAULT_CONFIG),

    io:format("~n========================================~n", []),
    io:format("DES Simulation - Erlang~n", []),
    io:format("========================================~n", []),
    io:format("Configuration:~n", []),
    io:format("  Grid size: ~px~p~n", [maps:get(grid_width, Config),
                                        maps:get(grid_height, Config)]),
    io:format("  Number of bots: ~p~n", [maps:get(num_bots, Config)]),
    io:format("  Bot speed: ~p unit/sec~n", [maps:get(bot_speed, Config)]),
    io:format("  Duration: ~p hour(s)~n", [maps:get(simulation_duration_hrs, Config)]),
    io:format("========================================~n~n", []),

    %% Start supervision tree
    case des_sim_sup:start_link(Config) of
        {ok, Pid} ->
            %% Spawn bots
            NumBots = maps:get(num_bots, Config),
            spawn_bots(NumBots),

            %% Give bots time to initialize and register
            timer:sleep(100),

            %% Start simulation
            des_scheduler:start_simulation(),

            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

spawn_bots(NumBots) ->
    lists:foreach(fun(N) ->
        BotId = list_to_atom("bot_" ++ integer_to_list(N)),
        {ok, _Pid} = des_bot_sup:start_bot(BotId)
    end, lists:seq(1, NumBots)).
