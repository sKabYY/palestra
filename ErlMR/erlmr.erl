-module(erlmr).
-export([mrgo/3,
         mk_input_format_from_list/1]).

% map: {k1, v1} -> [{k2, v2}]
% reduce: {k2, [v2]} -> [{k3, v3}]

mk_input_format_from_list(L) ->
    fun () ->
        case L of
            [] -> nil;
            [H|T] -> {H, mk_input_format_from_list(T)}
        end
    end.

% N: the max number of tasks
% Map: {K, V} X Emit -> void
% Emit: {K, V} -> void

mrgo(N, InputFormat, Map) ->
    InputFormatProc = spawn(fun () -> input_format_proc(InputFormat) end),
    MapEmitProc = spawn(fun () -> map_emit_proc() end),
    MapEmit = fun (KV) -> MapEmitProc ! {emit, KV} end,
    MapProcs = start_map_procs(N, self(), InputFormatProc, Map, MapEmit),
    wait(MapProcs),
    InputFormatProc ! stop,
    pull_and_stop(MapEmitProc).

wait([]) -> done;
wait(Pids) ->
    receive
        Pid -> wait([X || X <- Pids, X =/= Pid])
    end.

pull_and_stop(Pid) ->
    Pid ! {pull, self()},
    receive
        Data ->
            Pid ! stop,
            io:format("~w~n", [dict:to_list(Data)]),
            Data
    end.

map_emit_proc() ->
    map_emit_proc_iter(dict:new()).

map_emit_proc_iter(Bucket) ->
    receive
        {emit, {K, V}} ->
            map_emit_proc_iter(dict:append(K, V, Bucket));
        {pull, From} ->
            From ! Bucket,
            map_emit_proc_iter(Bucket);
        stop -> done
    end.

input_format_proc(InputFormat) ->
    receive
        {From, next} ->
            case InputFormat() of
                {KV, NextInputFormat} ->
                    From ! KV,
                    input_format_proc(NextInputFormat);
                _ ->
                    From ! stop,
                    input_format_proc(InputFormat)
            end;
        stop -> done
    end.

start_map_procs(0, _, _, _, _) -> [];
start_map_procs(N, MainPid, InputFormatProc, Map, Emit) when N > 0 ->
    Pid = spawn(fun () -> map_proc(MainPid, InputFormatProc, Map, Emit) end),
    [Pid|start_map_procs(N - 1, MainPid, InputFormatProc, Map, Emit)].

map_proc(MainPid, InputFormatProc, Map, Emit) ->
    InputFormatProc ! {self(), next},
    receive
        {K, V} ->
            Map({K, V}, Emit),
            map_proc(MainPid, InputFormatProc, Map, Emit);
        stop ->
            MainPid ! self()
    end.
