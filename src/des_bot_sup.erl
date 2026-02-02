%%%-------------------------------------------------------------------
%%% @doc Supervisor for bot processes
%%% @end
%%%-------------------------------------------------------------------
-module(des_bot_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_bot/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new bot with the given ID
start_bot(BotId) ->
    supervisor:start_child(?SERVER, [BotId]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    BotSpec = #{
        id => des_bot,
        start => {des_bot, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [des_bot]
    },

    {ok, {SupFlags, [BotSpec]}}.
