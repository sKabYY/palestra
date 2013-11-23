-module(fbm).
-export([go/1, loop/0]).

mainloop(Pid2, M) ->
    receive
        {Pid2, X} when X < M ->
            io:format("P1 ~w~n", [X]),
            Pid2 ! {self(), X + 1},
            mainloop(Pid2, M);
        _ -> true
    end.

go(M) when M > 0 ->
    Pid2 = spawn(fbm, loop, []),
    Pid2 ! {self(), 0},
    mainloop(Pid2, M),
    io:format("Fin.~n"),
    Pid2 ! stop.

loop() ->
    receive
        {Pid, X} ->
            Pid ! {self(), X},
            loop();
        stop -> true
    end.
