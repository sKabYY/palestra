-module(mrlib).
-export([mapreduce/3,
         default_map/2,
         default_reduce/1,
         info/2,
         pprint/1]).

% map: {k1, v1} -> [{k2, v2}]
% reduce: {k2, [v2]} -> {k2, v3}

% some usefull functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% log info
info(Template, Args) -> io:format("[~p]\t" ++ Template ++ "~n", [now()|Args]).

% pretty print
pprint(Datum) -> io:format("~p~n", [Datum]).

% default map
default_map({K, V}, Emit) -> Emit({K, V}).

% default reduce
default_reduce({K, ListOfV}) -> {K, ListOfV}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% N: the max number of workers
% Tasks: [Task]
% Task: {map, Map} or {reduce, Reduce}
% Map: {K, V} X Emit-> void
% Emit: {K, V} -> void
% Reduce: {K, [V]} -> V

mapreduce(_, _, []) -> {error, "The tasks is empty."};
mapreduce(N, InputList, Tasks) when is_list(Tasks) ->
    process_flag(trap_exit, true),
    WorkerPids = start_workern(N, fun input_proc/0),
    info("spliting input data", []),
    lists:foreach(mk_emit_hash(WorkerPids), InputList),
    info("go~~", []),
    mapreduce_iter(N, WorkerPids, Tasks).

mapreduce_iter(_, WorkerPids, [Task]) ->
    OutputPid = start_output(),
    start_and_wait(WorkerPids, Task, mk_emit_one(OutputPid)),
    get_output(OutputPid);
mapreduce_iter(N, WorkerPids, [Task|Tail]) ->
    OutputPids = start_workern(N, fun groupby_proc/0),
    start_and_wait(WorkerPids, Task, mk_emit_hash(OutputPids)),
    mapreduce_iter(N, OutputPids, Tail).

start_and_wait(WorkerPids, Task, Emit) ->
    info("start task: ~p", [Task]),
    lists:foreach(
      fun (Pid) -> Pid ! {Task, Emit} end,
      WorkerPids),
    wait_workers(WorkerPids),
    info("task done", []).

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

% make emit functions
mk_emit_one(Pid) ->
    fun (Datum) ->
            Pid ! {emit, Datum}
    end.

mk_emit_hash(Pids) ->
    PidArr = array:from_list(Pids),
    N = array:size(PidArr),
    fun ({K, V}) ->
            Idx = erlang:phash2(K, N),
            array:get(Idx, PidArr) ! {emit, {K, V}}
    end.

% receive proc
% Func: a callback function to handle other messages.
receive_proc(Store, Update, ToList, Func) ->
    receive
        {emit, Datum} ->
            receive_proc(Update(Datum, Store), Update, ToList, Func);
        OtherMsg ->
            Func(OtherMsg, ToList(Store))
    end.

% output proc
output_proc() ->
    receive_proc(
      [],
      fun (H, T) -> [H|T] end,
      fun lists:reverse/1,
      fun output_and_stop/2).

output_and_stop({output, From}, Output) ->
    info("sending data -- OUTPUT", []),
    From ! Output,
    info("ok -- OUTPUT", []).

start_output() -> spawn(fun output_proc/0).

get_output(Pid) ->
    Pid ! {output, self()},
    receive
        Output -> Output
    end.

% input proc
input_proc() ->
    receive_proc(
      [],
      fun (H, T) -> [H|T] end,
      fun lists:reverse/1,
      fun start_task_proc/2).

% groupby proc
groupby_proc() ->
    receive_proc(
      dict:new(),
      fun ({K, V}, Bucket) ->
              dict:update(K, fun (Vs) -> [V|Vs] end, [V], Bucket)
      end,
      fun dict:to_list/1,
      fun start_task_proc/2).

% start task proc
start_task_proc({{map, Map}, Emit}, Output) ->
    map_proc(Map, Output, Emit);
start_task_proc({{reduce, Reduce}, Emit}, Output) ->
    reduce_proc(Reduce, Output, Emit).

% map proc
map_proc(Map,InputList, Emit) ->
    task_proc(fun (KV) -> Map(KV, Emit) end, InputList).

% reduce proc
reduce_proc(Reduce, InputList, Emit) ->
    task_proc(fun (KV) -> Emit(Reduce(KV)) end, InputList).

% task proc
task_proc(_, []) -> ok;
task_proc(ProcessAndEmit, [KV|Tail]) ->
    ProcessAndEmit(KV),
    task_proc(ProcessAndEmit, Tail).
