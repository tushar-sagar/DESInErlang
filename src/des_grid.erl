%%%-------------------------------------------------------------------
%%% @doc Grid state management using ETS
%%% Manages bot positions on a 2D grid
%%% @end
%%%-------------------------------------------------------------------
-module(des_grid).
-behaviour(gen_server).

%% API
-export([start_link/1,
         get_config/0,
         random_position/0,
         update_bot_position/3,
         get_bot_position/1,
         get_all_bots/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(BOT_TABLE, des_bot_positions).

-record(state, {
    width :: pos_integer(),
    height :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Get grid configuration
get_config() ->
    gen_server:call(?SERVER, get_config).

%% @doc Generate a random position on the grid
random_position() ->
    gen_server:call(?SERVER, random_position).

%% @doc Update bot position
update_bot_position(BotId, Position, Destination) ->
    gen_server:cast(?SERVER, {update_position, BotId, Position, Destination}).

%% @doc Get bot position
get_bot_position(BotId) ->
    case ets:lookup(?BOT_TABLE, BotId) of
        [{BotId, Pos, Dest}] -> {ok, Pos, Dest};
        [] -> {error, not_found}
    end.

%% @doc Get all bot positions
get_all_bots() ->
    ets:tab2list(?BOT_TABLE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Config) ->
    %% Create ETS table for bot positions
    ets:new(?BOT_TABLE, [named_table, public, set, {read_concurrency, true}]),

    Width = maps:get(grid_width, Config, 50),
    Height = maps:get(grid_height, Config, 50),

    %% Seed random number generator
    rand:seed(exsplus, erlang:timestamp()),

    {ok, #state{width = Width, height = Height}}.

handle_call(get_config, _From, #state{width = W, height = H} = State) ->
    {reply, {W, H}, State};

handle_call(random_position, _From, #state{width = W, height = H} = State) ->
    X = rand:uniform(W) - 1,  % 0 to W-1
    Y = rand:uniform(H) - 1,  % 0 to H-1
    {reply, {X, Y}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_position, BotId, Position, Destination}, State) ->
    ets:insert(?BOT_TABLE, {BotId, Position, Destination}),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?BOT_TABLE),
    ok.
