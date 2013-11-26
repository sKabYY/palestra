-module(erlmr).
-export([mrgo/5,
         mk_input_format_from_list/1,
         pprint/1]).

pprint(Datum) -> io:format("~p~n", [Datum]).

% map: {k1, v1} -> [{k2, v2}]
% reduce: {k2, [v2]} -> {k2, v3}

mk_input_format_from_list(L) ->
    fun () ->
        case L of
            [] -> eof;
            [H|T] -> {H, mk_input_format_from_list(T)}
        end
    end.

% N: the max number of tasks
% Map: {K, V} X EmitIntermediate -> void
% EmitIntermediate: {K, V} -> void
% Reduce: {K, [V]} -> V
% InputFormat () -> {{K, V}, NextInputFormat} | eof
% OutputFormat {K, V} -> NextOutputFormat

mrgo(N, InputFormat, OutputFormat, Map, Reduce) ->
    InputFormatProc = spawn(fun () -> input_format_proc(InputFormat) end),
    EmitIntermediateProc = spawn(fun () -> emit_intermediate_proc() end),
    EmitIntermediate = fun (KV) -> EmitIntermediateProc ! {emit, KV} end,
    MapProcs = start_map_procs(N, self(), InputFormatProc, Map, EmitIntermediate),
    wait(MapProcs),
    stop(InputFormatProc),
    IntermediateData = pull_and_stop(EmitIntermediateProc),
    IntermediateInputProc =
        spawn(fun () ->
                      IntermediateInput = mk_input_format_from_list(IntermediateData),
                      input_format_proc(IntermediateInput)
              end),
    OutputFormatProc = spawn(fun () -> output_format_proc(OutputFormat) end),
    ReduceProcs = start_reduce_procs(N, self(), IntermediateInputProc, OutputFormatProc, Reduce),
    wait(ReduceProcs),
    stop(IntermediateInputProc),
    stop(OutputFormatProc).

% two types of processes: worker and tracker
% worker: map_proc and reduce_proc
% tracker: ...

wait([]) -> done;  % TODO
wait(Pids) ->
    receive
        Pid -> wait([X || X <- Pids, X =/= Pid])
    end.

stop(Pid) -> Pid ! stop, ok.

pull_and_stop(Pid) ->
    Pid ! {pull, self()},
    receive
        Datum ->
            Pid ! stop,
            Datum
    end.

emit_intermediate_proc() ->
    emit_intermediate_proc_iter(dict:new()).

emit_intermediate_proc_iter(Bucket) ->
    receive
        {emit, {K, V}} ->
            emit_intermediate_proc_iter(dict:append(K, V, Bucket));
        {pull, From} ->
            From ! dict:to_list(Bucket),
            emit_intermediate_proc_iter(Bucket);
        stop -> done
    end.

input_format_proc(InputFormat) ->
    receive
        {From, next} ->
            case InputFormat() of
                {KV, NextInputFormat} ->
                    From ! KV,
                    input_format_proc(NextInputFormat);
                eof ->
                    From ! eof,
                    input_format_proc(InputFormat)
            end;
        stop -> done
    end.

output_format_proc(OutputFormat) ->
    receive
        {output, KV} ->
            output_format_proc(OutputFormat(KV));
        stop -> done
    end.

start_map_procs(0, _, _, _, _) -> [];
start_map_procs(N, MainPid, InputFormatProc, Map, EmitIntermediate) when N > 0 ->
    Pid = spawn(fun () -> map_proc(MainPid, InputFormatProc, Map, EmitIntermediate) end),
    [Pid|start_map_procs(N - 1, MainPid, InputFormatProc, Map, EmitIntermediate)].

map_proc(MainPid, InputFormatProc, Map, EmitIntermediate) ->
    InputFormatProc ! {self(), next},
    receive
        {K, V} ->
            Map({K, V}, EmitIntermediate),
            map_proc(MainPid, InputFormatProc, Map, EmitIntermediate);
        eof ->
            MainPid ! self()
    end.

start_reduce_procs(0, _, _, _, _) -> [];
start_reduce_procs(N, MainPid, IntermediateInputProc, OutputFormatProc, Reduce) ->
    Pid = spawn(fun () -> reduce_proc(MainPid, IntermediateInputProc, OutputFormatProc, Reduce) end),
    [Pid|start_reduce_procs(N - 1, MainPid, IntermediateInputProc, OutputFormatProc, Reduce)].

reduce_proc(MainPid, IntermediateInputProc, OutputFormatProc, Reduce) ->
    IntermediateInputProc ! {self(), next},
    receive
        {K, ListOfV} ->
            V = Reduce({K, ListOfV}),
            OutputFormatProc ! {output, {K, V}},
            reduce_proc(MainPid, IntermediateInputProc, OutputFormatProc, Reduce);
        eof ->
            MainPid ! self()
    end.
