%%%-------------------------------------------------------------------
%%% @doc Main supervisor for the DES simulation
%%% @end
%%%-------------------------------------------------------------------
-module(des_sim_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Config) ->
    SupFlags = #{
        strategy => one_for_all,  % If one fails, restart all
        intensity => 5,
        period => 60
    },

    %% Grid must start first
    GridSpec = #{
        id => des_grid,
        start => {des_grid, start_link, [Config]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [des_grid]
    },

    %% Scheduler starts second
    SchedulerSpec = #{
        id => des_scheduler,
        start => {des_scheduler, start_link, [Config]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [des_scheduler]
    },

    %% Bot supervisor starts last
    BotSupSpec = #{
        id => des_bot_sup,
        start => {des_bot_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [des_bot_sup]
    },

    {ok, {SupFlags, [GridSpec, SchedulerSpec, BotSupSpec]}}.
