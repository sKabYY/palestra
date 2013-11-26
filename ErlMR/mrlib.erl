-module(mrlib).
-export([mapreduce/5,
         mk_input_format_from_list/1,
         default_map/2,
         default_reduce/1,
         default_output_format/1,
         pprint/1]).

% map: {k1, v1} -> [{k2, v2}]
% reduce: {k2, [v2]} -> {k2, v3}

% some usefull functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pretty print
pprint(Datum) -> io:format("~p~n", [Datum]).

% make input format from a list
mk_input_format_from_list(L) ->
    fun () ->
        case L of
            [] -> eof;
            [H|T] -> {H, mk_input_format_from_list(T)}
        end
    end.

% default map
default_map({K, V}, Emit) -> Emit({K, V}).

% default reduce
default_reduce({_K, ListOfV}) -> ListOfV.

% default output_format
default_output_format({K, V}) ->
    pprint({K, V}),
    fun default_output_format/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% N: the max number of workers
% Map: {K, V} X EmitIntermediate -> void
% EmitIntermediate: {K, V} -> void
% Reduce: {K, [V]} -> V
% InputFormat () -> {{K, V}, NextInputFormat} | eof
%     InputFormat may have with side-effect.
%     The return value of InputFormat after it has returned eof is unspecified.
% OutputFormat {K, V} -> NextOutputFormat

mapreduce(N, InputFormat, OutputFormat, Map, Reduce) ->
    process_flag(trap_exit, true),
    InputFormatProc = start_tracker(
                        fun () ->
                                input_format_proc(InputFormat)
                        end),
    EmitIntermediateProc = start_tracker(
                             fun () ->
                                     emit_intermediate_proc()
                             end),
    EmitIntermediate = fun (KV) -> EmitIntermediateProc ! {emit, KV} end,
    MapProcs = start_workern(
                 N,
                 fun () ->
                         map_proc(Map,
                                  InputFormatProc,
                                  EmitIntermediate)
                 end),
    wait_workers(MapProcs),
    stop_tracker(InputFormatProc),
    IntermediateData = pull_and_stop_tracker(EmitIntermediateProc),
    IntermediateInput = mk_input_format_from_list(IntermediateData),
    IntermediateInputProc = start_tracker(
                              fun () ->
                                      input_format_proc(IntermediateInput)
                              end),
    OutputFormatProc = start_tracker(
                         fun () ->
                                 output_format_proc(OutputFormat)
                         end),
    ReduceProcs = start_workern(
                    N,
                    fun() ->
                            reduce_proc(Reduce,
                                        IntermediateInputProc,
                                        OutputFormatProc)
                    end),
    wait_workers(ReduceProcs),
    stop_tracker(IntermediateInputProc),
    stop_tracker(OutputFormatProc).

% two types of processes: worker and tracker
% worker: map_proc and reduce_proc, stop after receive 'eof'
% tracker: stop after receive 'stop'

% tracker
start_tracker(F) -> spawn(F).

stop_tracker(Pid) -> Pid ! stop, ok.

pull_and_stop_tracker(Pid) ->
    Pid ! {pull, self()},
    receive
        Datum ->
            Pid ! stop,
            Datum
    end.

% worker
start_workern(N, F) ->
    start_workern_iter([], N, F).

start_workern_iter(Acc, 0, _) -> Acc;
start_workern_iter(Acc, N, F) ->
    Pid = spawn_link(F),
    start_workern_iter([Pid|Acc], N - 1, F).

wait_workers([]) -> ok;
wait_workers(Pids) ->
    receive
        {'EXIT', Pid, _Why} ->
            wait_workers([X || X <- Pids, X =/= Pid])
    end.

% emit proc
emit_intermediate_proc() ->
    emit_intermediate_proc_iter(dict:new()).

emit_intermediate_proc_iter(Bucket) ->
    receive
        {emit, {K, V}} ->
            emit_intermediate_proc_iter(dict:append(K, V, Bucket));
        {pull, From} ->
            From ! dict:to_list(Bucket),
            emit_intermediate_proc_iter(Bucket);
        stop -> ok
    end.

% a proc always sends eof
eof_proc() ->
    receive
        {From, next} ->
            From ! eof,
            eof_proc();
        stop -> ok
    end.

% input format proc
input_format_proc(InputFormat) ->
    receive
        {From, next} ->
            case InputFormat() of
                {KV, NextInputFormat} ->
                    From ! KV,
                    input_format_proc(NextInputFormat);
                eof ->
                    From ! eof,
                    eof_proc()
            end;
        stop -> ok
    end.

% output format proc
output_format_proc(OutputFormat) ->
    receive
        {output, KV} ->
            output_format_proc(OutputFormat(KV));
        stop -> ok
    end.

% map proc
map_proc(Map, InputFormatProc, EmitIntermediate) ->
    InputFormatProc ! {self(), next},
    receive
        {K, V} ->
            Map({K, V}, EmitIntermediate),
            map_proc(Map, InputFormatProc, EmitIntermediate);
        eof -> ok
    end.

% reduce proc
reduce_proc(Reduce, IntermediateInputProc, OutputFormatProc) ->
    IntermediateInputProc ! {self(), next},
    receive
        {K, ListOfV} ->
            V = Reduce({K, ListOfV}),
            OutputFormatProc ! {output, {K, V}},
            reduce_proc(Reduce, IntermediateInputProc, OutputFormatProc);
        eof -> ok
    end.
