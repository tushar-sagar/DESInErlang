%%%-------------------------------------------------------------------
%%% @doc DES Simulation OTP Application
%%% Configuration loaded from config/sys.local.config
%%% @end
%%%-------------------------------------------------------------------
-module(des_sim_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([run/0, run/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Run simulation with config from sys.local.config
run() ->
    run(#{}).

%% @doc Run simulation with custom config (overrides sys.local.config)
run(Overrides) ->
    %% Read from application env (loaded from sys.local.config)
    Config = load_config(Overrides),
    application:set_env(des_sim, config, Config),
    application:ensure_all_started(des_sim).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    %% Load config from application env or use passed config
    Config = case application:get_env(des_sim, config) of
        {ok, C} when is_map(C) -> C;
        _ -> load_config(#{})
    end,

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

%% @doc Load configuration from application env with defaults
load_config(Overrides) ->
    Defaults = #{
        grid_width => 50,
        grid_height => 50,
        num_bots => 10,
        bot_speed => 1,
        simulation_duration_hrs => 1
    },

    %% Read individual keys from application env (set by sys.config)
    FromEnv = #{
        grid_width => get_env(grid_width, maps:get(grid_width, Defaults)),
        grid_height => get_env(grid_height, maps:get(grid_height, Defaults)),
        num_bots => get_env(num_bots, maps:get(num_bots, Defaults)),
        bot_speed => get_env(bot_speed, maps:get(bot_speed, Defaults)),
        simulation_duration_hrs => get_env(simulation_duration_hrs,
                                           maps:get(simulation_duration_hrs, Defaults))
    },

    %% Merge: Defaults < FromEnv < Overrides
    maps:merge(FromEnv, Overrides).

get_env(Key, Default) ->
    case application:get_env(des_sim, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

spawn_bots(NumBots) ->
    lists:foreach(fun(N) ->
        BotId = list_to_atom("bot_" ++ integer_to_list(N)),
        {ok, _Pid} = des_bot_sup:start_bot(BotId)
    end, lists:seq(1, NumBots)).
