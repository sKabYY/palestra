-module(ring).
-export([go/2, proc_creator/2, proc/1]).

proc(NextPid) ->
    SelfId = self(),
    receive
        {_, 0} ->
            io:format("[~w]Stop!~n", [self()]),
            NextPid ! {stop, 0};
        {send, Msg} ->
            io:format("[~w]Gooo: to:~w ~w~n", [self(), NextPid, Msg]),
            NextPid ! {{self(), self()}, Msg},
            proc(NextPid);
        {{From, SelfId}, Msg} ->
            io:format("[~w]Ring: from:~w to:~w ~w~n", [self(), From, NextPid, Msg]),
            proc(NextPid);
        {{From, StartId}, Msg} ->
            io:format("[~w]Tran: from:~w to:~w ~w~n", [self(), From, NextPid, Msg]),
            NextPid ! {{self(), StartId}, Msg},
            proc(NextPid)
    end.

proc_creator(N, MainPid) when N >= 2 ->
    Pids = create(N - 1, self()),
    [NextPid|_] = Pids,
    MainPid ! [self()|Pids],
    proc(NextPid).

create(1, FirstPid) ->
    Pid = spawn(ring, proc, [FirstPid]),
    [Pid];
create(N, FirstPid) when N > 1 ->
    Pids = create(N - 1, FirstPid),
    [NextPid|_] = Pids,
    Pid = spawn(ring, proc, [NextPid]),
    [Pid|Pids].

go(N, M) ->
    Pid = spawn(ring, proc_creator, [N, self()]),
    send(Pid, M).

send(_, -1) -> done;
send(Pid, M) when M >= 0 ->
    Pid ! {send, M},
    send(Pid, M - 1).
