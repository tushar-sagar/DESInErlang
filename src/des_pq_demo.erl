%%%-------------------------------------------------------------------
%%% @doc Visual demonstration of Priority Queue (gb_trees) in DES
%%% Run: des_pq_demo:run().
%%% @end
%%%-------------------------------------------------------------------
-module(des_pq_demo).
-export([run/0]).

run() ->
    io:format("~n"),
    io:format("+--------------------------------------------------------------+~n"),
    io:format("|     PRIORITY QUEUE (gb_trees) VISUAL DEMONSTRATION           |~n"),
    io:format("+--------------------------------------------------------------+~n~n"),

    %% Start with empty queue
    io:format("=== Step 1: Create Empty Priority Queue ===~n"),
    PQ0 = gb_trees:empty(),
    print_queue(PQ0),

    %% Simulate 3 bots scheduling their first events
    io:format("~n=== Step 2: Bot1 schedules event at T=0.0 ===~n"),
    io:format("  Code: gb_trees:insert({0.0, 1}, {bot1, move}, Queue)~n"),
    PQ1 = gb_trees:insert({0.0, 1}, {bot1, move}, PQ0),
    print_queue(PQ1),

    io:format("~n=== Step 3: Bot2 schedules event at T=0.0 ===~n"),
    io:format("  Code: gb_trees:insert({0.0, 2}, {bot2, move}, Queue)~n"),
    PQ2 = gb_trees:insert({0.0, 2}, {bot2, move}, PQ1),
    print_queue(PQ2),

    io:format("~n=== Step 4: Bot3 schedules event at T=0.0 ===~n"),
    io:format("  Code: gb_trees:insert({0.0, 3}, {bot3, move}, Queue)~n"),
    PQ3 = gb_trees:insert({0.0, 3}, {bot3, move}, PQ2),
    print_queue(PQ3),

    io:format("~n+--------------------------------------------------------------+~n"),
    io:format("|                  SIMULATION LOOP BEGINS                       |~n"),
    io:format("+--------------------------------------------------------------+~n"),

    %% Process first event
    io:format("~n=== Step 5: Pop smallest event (T=0.0, Bot1) ===~n"),
    io:format("  Code: gb_trees:take_smallest(Queue)~n"),
    {{Time1, _}, Event1, PQ4} = gb_trees:take_smallest(PQ3),
    io:format("  -> Popped: Time=~p, Event=~p~n", [Time1, Event1]),
    io:format("  -> Advance simulation time: T = ~p~n", [Time1]),
    print_queue(PQ4),

    %% Bot1 processes and schedules next event at T+1
    io:format("~n=== Step 6: Bot1 processes event, schedules next at T=1.0 ===~n"),
    io:format("  Code: gb_trees:insert({1.0, 4}, {bot1, move}, Queue)~n"),
    PQ5 = gb_trees:insert({1.0, 4}, {bot1, move}, PQ4),
    print_queue(PQ5),

    %% Process second event
    io:format("~n=== Step 7: Pop smallest event (T=0.0, Bot2) ===~n"),
    {{Time2, _}, Event2, PQ6} = gb_trees:take_smallest(PQ5),
    io:format("  -> Popped: Time=~p, Event=~p~n", [Time2, Event2]),
    print_queue(PQ6),

    %% Bot2 schedules at T=1.0
    io:format("~n=== Step 8: Bot2 schedules next at T=1.0 ===~n"),
    PQ7 = gb_trees:insert({1.0, 5}, {bot2, move}, PQ6),
    print_queue(PQ7),

    %% Process third event
    io:format("~n=== Step 9: Pop smallest event (T=0.0, Bot3) ===~n"),
    {{Time3, _}, Event3, PQ8} = gb_trees:take_smallest(PQ7),
    io:format("  -> Popped: Time=~p, Event=~p~n", [Time3, Event3]),
    print_queue(PQ8),

    %% Bot3 reaches destination, schedules at T=0.5 (different timing!)
    io:format("~n=== Step 10: Bot3 reached destination! Schedules at T=0.5 ===~n"),
    io:format("  (Bot3 is faster or closer to destination)~n"),
    PQ9 = gb_trees:insert({0.5, 6}, {bot3, reached_dest}, PQ8),
    print_queue(PQ9),

    %% Now watch the order
    io:format("~n+--------------------------------------------------------------+~n"),
    io:format("|   *** NOTICE: Events processed by TIME ORDER! ***            |~n"),
    io:format("|   Bot3's event at T=0.5 comes BEFORE Bot1/Bot2 at T=1.0      |~n"),
    io:format("+--------------------------------------------------------------+~n"),

    io:format("~n=== Step 11: Pop next event ===~n"),
    {{Time4, _}, Event4, PQ10} = gb_trees:take_smallest(PQ9),
    io:format("  -> Time JUMPS from 0.0 to ~p~n", [Time4]),
    io:format("  -> Event: ~p~n", [Event4]),
    print_queue(PQ10),

    io:format("~n=== Step 12: Pop next event ===~n"),
    {{Time5, _}, Event5, PQ11} = gb_trees:take_smallest(PQ10),
    io:format("  -> Time advances to ~p~n", [Time5]),
    io:format("  -> Event: ~p~n", [Event5]),
    print_queue(PQ11),

    io:format("~n=== Step 13: Pop next event ===~n"),
    {{Time6, _}, Event6, _PQ12} = gb_trees:take_smallest(PQ11),
    io:format("  -> Time stays at ~p (same time, different bot)~n", [Time6]),
    io:format("  -> Event: ~p~n", [Event6]),

    io:format("~n+--------------------------------------------------------------+~n"),
    io:format("|                         KEY POINTS                            |~n"),
    io:format("+--------------------------------------------------------------+~n"),
    io:format("|  1. Events sorted by TIME (smallest first)                   |~n"),
    io:format("|  2. Key = {Time, Counter} ensures unique keys at same time   |~n"),
    io:format("|  3. Time JUMPS from event to event (not fixed increments)    |~n"),
    io:format("|  4. gb_trees gives O(log n) insert and take_smallest         |~n"),
    io:format("|  5. Each bot schedules its OWN next event after processing   |~n"),
    io:format("+--------------------------------------------------------------+~n~n"),

    ok.

print_queue(Queue) ->
    case gb_trees:is_empty(Queue) of
        true ->
            io:format("~n  Priority Queue: [EMPTY]~n");
        false ->
            Events = gb_trees:to_list(Queue),
            io:format("~n  Priority Queue (sorted by time):~n"),
            io:format("  +-------------+---------+--------------------+~n"),
            io:format("  |    Time     | Counter |       Event        |~n"),
            io:format("  +-------------+---------+--------------------+~n"),
            lists:foreach(fun({{Time, Counter}, Event}) ->
                io:format("  | ~11.1f | ~7w | ~-18w |~n", [Time, Counter, Event])
            end, Events),
            io:format("  +-------------+---------+--------------------+~n"),
            io:format("    ^ Next event to process (smallest time)~n")
    end.
