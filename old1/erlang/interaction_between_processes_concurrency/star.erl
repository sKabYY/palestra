-module(star).
-export([go/2, proc/0]).

go(N, M) when N > 0, M > 0 ->
    Pids = create(N),
    io:format("Pids: ~w~n", [Pids]),
    send(Pids, Pids, M),
    receive_m(Pids).

create(0) -> [];
create(N) when N > 0 ->
    Pid = spawn(star, proc, []),
    [Pid|create(N - 1)].

receive_m([]) -> done;
receive_m(Pids) ->
    receive
        {From, stop} ->
            io:format("[Main] from:~w msg:~w~n", [From, stop]),
            receive_m([Pid || Pid <- Pids, Pid =/= From]);
        {From, Msg} ->
            io:format("[Main] from:~w msg:~w~n", [From, Msg]),
            receive_m(Pids)
    end.

send(Pids, _, 0) -> stopall(Pids);
send(Pids, [], M) when M > 0 -> send(Pids, Pids, M - 1);
send(Pids, [Pid|Rest], M) ->
    Pid ! {self(), M},
    send(Pids, Rest, M).

stopall([]) -> done;
stopall([Pid|Rest]) ->
    Pid ! {self(), stop},
    stopall(Rest).

proc() ->
    receive
        {From, stop} ->
            io:format("[~w] ~w\n", [self(), stop]),
            From ! {self(), stop},
            done;
        {From, Msg} ->
            io:format("[~w] ~w\n", [self(), Msg]),
            From ! {self(), Msg},
            proc()
    end.
