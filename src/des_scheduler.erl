%%%-------------------------------------------------------------------
%%% @doc Discrete Event Simulation Scheduler
%%% True event-driven DES with priority queue (like SimPy)
%%% @end
%%%-------------------------------------------------------------------
-module(des_scheduler).
-behaviour(gen_server).

%% API
-export([start_link/1,
         schedule_event/3,
         start_simulation/0,
         get_stats/0,
         get_current_time/0,
         get_base_monotonic/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {
    current_time :: float(),
    end_time :: float(),
    event_queue :: gb_trees:tree(),  % Priority queue: {Time, Counter} -> Event
    event_counter :: non_neg_integer(),  % For unique keys at same time
    running :: boolean(),
    start_wall_time :: integer(),
    base_monotonic :: integer(),  % System time (Unix epoch ms) at simulation start for reservation timestamps
    total_events :: non_neg_integer(),
    bots :: [{pid(), atom()}]  % For final stats
}).

%% Event record
-record(event, {
    type :: atom(),
    pid :: pid(),
    data :: term()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Schedule an event at a specific simulation time
%% Called by bots to schedule their next action
-spec schedule_event(float(), atom(), term()) -> ok.
schedule_event(Time, EventType, Data) ->
    gen_server:cast(?SERVER, {schedule, Time, EventType, self(), Data}).

%% @doc Start the simulation
start_simulation() ->
    gen_server:cast(?SERVER, start_simulation).

%% @doc Get simulation statistics
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Get current simulation time
get_current_time() ->
    gen_server:call(?SERVER, get_current_time).

%% @doc Get base monotonic time for reservation timestamp mapping
get_base_monotonic() ->
    gen_server:call(?SERVER, get_base_monotonic).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Config) ->
    DurationHrs = maps:get(simulation_duration_hrs, Config, 1),
    EndTime = DurationHrs * 3600.0,  % Convert hours to seconds

    {ok, #state{
        current_time = 0.0,
        end_time = EndTime,
        event_queue = gb_trees:empty(),
        event_counter = 0,
        running = false,
        start_wall_time = 0,
        base_monotonic = 0,
        total_events = 0,
        bots = []
    }}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        current_time => State#state.current_time,
        end_time => State#state.end_time,
        total_events => State#state.total_events,
        queue_size => gb_trees:size(State#state.event_queue),
        running => State#state.running
    },
    {reply, Stats, State};

handle_call(get_current_time, _From, State) ->
    {reply, State#state.current_time, State};

handle_call(get_base_monotonic, _From, State) ->
    {reply, State#state.base_monotonic, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({schedule, Time, EventType, Pid, Data}, State) ->
    #state{event_queue = Queue, event_counter = Counter, bots = Bots} = State,

    %% Create unique key: {Time, Counter}
    Key = {Time, Counter},
    Event = #event{type = EventType, pid = Pid, data = Data},
    NewQueue = gb_trees:insert(Key, Event, Queue),

    %% Track bot if this is a new bot (first event carries bot_id in data)
    NewBots = case maps:find(bot_id, Data) of
        {ok, BotId} -> [{Pid, BotId} | Bots];
        error -> Bots
    end,

    {noreply, State#state{
        event_queue = NewQueue,
        event_counter = Counter + 1,
        bots = NewBots
    }};

handle_cast(start_simulation, State) ->
    io:format("~n=== DES Simulation Started (Event-Driven) ===~n", []),
    io:format("End time: ~p simulation seconds (~.2f hours)~n",
              [trunc(State#state.end_time), State#state.end_time / 3600]),
    io:format("Events in queue: ~p~n~n", [gb_trees:size(State#state.event_queue)]),

    WallStart = erlang:monotonic_time(millisecond),
    %% Use system_time for base_monotonic (Unix epoch ms) for cross-node reservation timestamps
    BaseTime = erlang:system_time(millisecond),
    NewState = State#state{
        running = true,
        start_wall_time = WallStart,
        base_monotonic = BaseTime
    },

    %% Start processing events
    self() ! process_next_event,

    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(process_next_event, #state{running = false} = State) ->
    {noreply, State};

handle_info(process_next_event, #state{event_queue = Queue} = State) ->
    case gb_trees:is_empty(Queue) of
        true ->
            %% No more events
            finish_simulation(State),
            {noreply, State#state{running = false}};
        false ->
            %% Get next event (smallest time)
            {{Time, _Counter}, Event, NewQueue} = gb_trees:take_smallest(Queue),

            %% Check if we've exceeded end time
            case Time > State#state.end_time of
                true ->
                    finish_simulation(State),
                    {noreply, State#state{running = false}};
                false ->
                    %% Advance simulation time and process event
                    NewState = process_event(Event, State#state{
                        current_time = Time,
                        event_queue = NewQueue,
                        total_events = State#state.total_events + 1
                    }),

                    %% Continue processing
                    self() ! process_next_event,
                    {noreply, NewState}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Process a single event by sending it to the target process
%% All events are synchronous to ensure reservation booking completes before next event
%% Injects base_monotonic into event data so bots don't need to call back to scheduler
process_event(#event{type = Type, pid = Pid, data = Data}, State) ->
    EnrichedData = Data#{base_monotonic => State#state.base_monotonic},
    try
        gen_server:call(Pid, {event, Type, State#state.current_time, EnrichedData}, infinity)
    catch
        exit:{noproc, _} -> ok;
        exit:{timeout, _} -> ok
    end,
    State.

finish_simulation(State) ->
    #state{
        current_time = CurrentTime,
        end_time = EndTime,
        total_events = TotalEvents,
        start_wall_time = WallStart,
        bots = Bots
    } = State,

    WallEnd = erlang:monotonic_time(millisecond),
    WallDuration = (WallEnd - WallStart) / 1000,

    SimTime = min(CurrentTime, EndTime),

    io:format("~n~s~n", [string:copies("=", 60)]),
    io:format("                    SIMULATION COMPLETE~n", []),
    io:format("~s~n~n", [string:copies("=", 60)]),

    %% Time Statistics
    io:format("=== Time Statistics ===~n", []),
    io:format("  Simulation time:    ~p seconds (~.2f hours)~n", [trunc(SimTime), SimTime / 3600]),
    io:format("  Wall-clock time:    ~.3f seconds~n", [WallDuration]),
    io:format("  Speed ratio:        ~.1fx real-time~n", [SimTime / max(WallDuration, 0.001)]),
    io:format("  Events processed:   ~p~n~n", [TotalEvents]),

    %% Wait for bots to finish processing
    timer:sleep(100),

    %% Collect all bot statistics
    AllStats = lists:filtermap(fun({BotPid, _}) ->
        try
            Stats = des_bot:get_stats(BotPid),
            {true, Stats}
        catch
            _:_ -> false
        end
    end, Bots),

    %% Print individual bot statistics
    io:format("=== Bot Statistics ===~n", []),
    io:format("Bot        Trips    Moves    PathReqs   AvgPath    A*Time(ms)~n", []),
    io:format("~s~n", [string:copies("-", 65)]),

    lists:foreach(fun(Stats) ->
        #{id := Id,
          trips_completed := Trips,
          total_moves := Moves,
          path_requests := PathReqs,
          avg_path_length := AvgPath,
          path_compute_time_us := PathTimeUs} = Stats,
        PathTimeMs = PathTimeUs / 1000,
        io:format("~-10s ~-8w ~-8w ~-10w ~-10.1f ~-10.2f~n",
                  [atom_to_list(Id), Trips, Moves, PathReqs, AvgPath, PathTimeMs])
    end, AllStats),

    %% Aggregate statistics
    TotalTrips = lists:sum([maps:get(trips_completed, S) || S <- AllStats]),
    TotalMoves = lists:sum([maps:get(total_moves, S) || S <- AllStats]),
    TotalPathReqs = lists:sum([maps:get(path_requests, S) || S <- AllStats]),
    TotalPathTimeUs = lists:sum([maps:get(path_compute_time_us, S) || S <- AllStats]),
    TotalPathLen = lists:sum([maps:get(total_path_length, S) || S <- AllStats]),
    TotalDistance = lists:sum([maps:get(total_distance, S) || S <- AllStats]),
    TotalResFails = lists:sum([maps:get(reservation_failures, S, 0) || S <- AllStats]),

    io:format("~s~n", [string:copies("-", 70)]),

    io:format("~n=== Aggregate Statistics ===~n", []),
    io:format("  Total bots:              ~p~n", [length(AllStats)]),
    io:format("  Total trips completed:   ~p~n", [TotalTrips]),
    io:format("  Total moves:             ~p~n", [TotalMoves]),
    io:format("  Total distance traveled: ~p cells~n", [TotalDistance]),
    io:format("  Total path requests:     ~p~n", [TotalPathReqs]),
    io:format("  Total A* compute time:   ~.2f ms~n", [TotalPathTimeUs / 1000.0]),
    io:format("  Avg path length:         ~.1f cells~n", [float(safe_div(TotalPathLen, TotalPathReqs))]),
    io:format("  Avg A* time per request: ~.2f us~n", [float(safe_div(TotalPathTimeUs, TotalPathReqs))]),
    io:format("  Reservation conflicts:   ~p~n", [TotalResFails]),
    io:format("  Events per wall-second:  ~p~n", [trunc(TotalEvents / max(WallDuration, 0.001))]),

    io:format("~n~s~n", [string:copies("=", 60)]),
    ok.

safe_div(_, 0) -> 0.0;
safe_div(A, B) -> float(A) / float(B).
