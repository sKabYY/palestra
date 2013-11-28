-module(sort).
-export([test/0]).

wsort([]) -> [];
wsort([H|T]) ->
    wsort([X || X <- T, X < H]) ++ [H] ++ wsort([X || X <- T, X >= H]).

fsort([]) -> [];
fsort([H|T]) ->
    fsort(lists:filter(fun (X) -> X < H end, T))
        ++ [H] ++
        fsort(lists:filter(fun (X) -> X >= H end, T)).

qsort([]) -> [];
qsort([H|T]) ->
    {L, R} = lists:partition(fun (X) -> X < H end, T),
    qsort(L) ++ [H] ++ qsort(R).

psort_proc(Parent, []) -> Parent ! {self(), []};
psort_proc(Parent, [E]) -> Parent ! {self(), [E]};
psort_proc(Parent, [H|T]) ->
    {L, R} = lists:foldr(
               fun (X, {L0, R0}) ->
                       if
                           X < H -> {[X|L0], R0};
                           true -> {L0, [X|R0]}
                       end
               end,
               {[], []}, T),
    Me = self(),
    LPid = spawn_link(fun () -> psort_proc(Me, L) end),
    RPid = spawn_link(fun () -> psort_proc(Me, R) end),
    List = receive
               {LPid, SL} ->
                   receive
                       {_, SR} -> SL ++ [H] ++ SR
                   end;
               {RPid, SR} ->
                   receive
                       {_, SL} -> SL ++ [H] ++ SR
                   end
           end,
    Parent ! {self(), List}.

psort(List) ->
    psort_proc(self(), List),
    receive
        {_, S} -> S
    end.


test_one(F, Input, Answer) ->
    {S1, S2, S3} = now(),
    Result = Answer == F(Input),
    {E1, E2, E3} = now(),
    {D1, D2, D3} = {E1 - S1, E2 - S2, E3 - S3},
    Delta = D1 * 1000000 + D2 + D3 / 1000000,
    io:format("~p: ~p, time: ~ps~n", [F, Result, Delta]).

randlist(N) -> randlist_acc([], N).
randlist_acc(Acc, 0) -> Acc;
randlist_acc(Acc, N) -> randlist_acc([random:uniform()|Acc], N - 1).

test() ->
    List = randlist(1000000),
    Answer = lists:sort(List),
    lists:foreach(
      fun (F) -> test_one(F, List, Answer) end,
      [fun lists:sort/1, fun fsort/1, fun wsort/1, fun qsort/1, fun psort/1]).
