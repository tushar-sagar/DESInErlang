%%%-------------------------------------------------------------------
%%% @doc A* Pathfinding Algorithm for 4-directional grid movement
%%% @end
%%%-------------------------------------------------------------------
-module(des_astar).

-export([find_path/4]).

%% @doc Find path from Start to Goal on a grid of given size
%% Returns {ok, Path} where Path is list of coordinates from Start to Goal
%% Returns {error, no_path} if no path exists
-spec find_path(Start, Goal, GridWidth, GridHeight) -> {ok, Path} | {error, no_path} when
    Start :: {integer(), integer()},
    Goal :: {integer(), integer()},
    GridWidth :: pos_integer(),
    GridHeight :: pos_integer(),
    Path :: [{integer(), integer()}].
find_path(Start, Goal, GridWidth, GridHeight) ->
    %% OpenSet: priority queue as [{F, G, Pos}] sorted by F
    %% G = cost from start, F = G + heuristic
    G0 = 0,
    H0 = manhattan_distance(Start, Goal),
    F0 = G0 + H0,
    OpenSet = [{F0, G0, Start}],
    ClosedSet = sets:new(),
    CameFrom = #{},
    GScores = #{Start => G0},

    case astar_loop(OpenSet, ClosedSet, CameFrom, GScores, Goal, GridWidth, GridHeight) of
        {ok, FinalCameFrom} ->
            Path = reconstruct_path(FinalCameFrom, Goal, [Goal]),
            {ok, Path};
        {error, no_path} ->
            {error, no_path}
    end.

%% Main A* loop
astar_loop([], _ClosedSet, _CameFrom, _GScores, _Goal, _W, _H) ->
    {error, no_path};
astar_loop([{_F, _G, Current} | RestOpen], ClosedSet, CameFrom, GScores, Goal, W, H) ->
    case Current of
        Goal ->
            {ok, CameFrom};
        _ ->
            case sets:is_element(Current, ClosedSet) of
                true ->
                    %% Already processed, skip
                    astar_loop(RestOpen, ClosedSet, CameFrom, GScores, Goal, W, H);
                false ->
                    NewClosedSet = sets:add_element(Current, ClosedSet),
                    Neighbors = get_neighbors(Current, W, H),
                    {NewOpenSet, NewCameFrom, NewGScores} =
                        process_neighbors(Neighbors, Current, RestOpen, CameFrom,
                                         GScores, NewClosedSet, Goal),
                    astar_loop(NewOpenSet, NewClosedSet, NewCameFrom, NewGScores, Goal, W, H)
            end
    end.

%% Process all neighbors of current node
process_neighbors([], _Current, OpenSet, CameFrom, GScores, _ClosedSet, _Goal) ->
    {OpenSet, CameFrom, GScores};
process_neighbors([Neighbor | Rest], Current, OpenSet, CameFrom, GScores, ClosedSet, Goal) ->
    case sets:is_element(Neighbor, ClosedSet) of
        true ->
            process_neighbors(Rest, Current, OpenSet, CameFrom, GScores, ClosedSet, Goal);
        false ->
            CurrentG = maps:get(Current, GScores),
            TentativeG = CurrentG + 1,  % Cost of 1 for each step
            NeighborG = maps:get(Neighbor, GScores, infinity),
            case TentativeG < NeighborG of
                true ->
                    NewCameFrom = CameFrom#{Neighbor => Current},
                    NewGScores = GScores#{Neighbor => TentativeG},
                    H = manhattan_distance(Neighbor, Goal),
                    F = TentativeG + H,
                    NewOpenSet = insert_sorted({F, TentativeG, Neighbor}, OpenSet),
                    process_neighbors(Rest, Current, NewOpenSet, NewCameFrom,
                                     NewGScores, ClosedSet, Goal);
                false ->
                    process_neighbors(Rest, Current, OpenSet, CameFrom, GScores, ClosedSet, Goal)
            end
    end.

%% Get valid 4-directional neighbors
get_neighbors({X, Y}, Width, Height) ->
    Candidates = [
        {X - 1, Y},  % Left
        {X + 1, Y},  % Right
        {X, Y - 1},  % Up
        {X, Y + 1}   % Down
    ],
    [Pos || Pos = {PX, PY} <- Candidates,
            PX >= 0, PX < Width,
            PY >= 0, PY < Height].

%% Manhattan distance heuristic
manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

%% Insert into sorted list (by F value)
insert_sorted(Item, []) ->
    [Item];
insert_sorted({F1, _, _} = Item, [{F2, _, _} | _] = List) when F1 =< F2 ->
    [Item | List];
insert_sorted(Item, [H | T]) ->
    [H | insert_sorted(Item, T)].

%% Reconstruct path from came_from map
reconstruct_path(CameFrom, Current, Path) ->
    case maps:get(Current, CameFrom, undefined) of
        undefined ->
            Path;
        Previous ->
            reconstruct_path(CameFrom, Previous, [Previous | Path])
    end.
