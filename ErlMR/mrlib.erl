-module(mrlib).
-export([mapreduce/3,
         default_map/2,
         default_reduce/1,
         pprint/1]).

% map: {k1, v1} -> [{k2, v2}]
% reduce: {k2, [v2]} -> {k2, v3}

% some usefull functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    mapreduce_iter(N, InputList, Tasks).

mapreduce_iter(N, InputList, [Task]) ->
    start_task(N, InputList, Task, fun output_proc/0);
mapreduce_iter(N, InputList, [Task|Tail]) ->
    IntermediateList = start_task(N, InputList, Task, fun groupby_proc/0),
    mapreduce_iter(N, IntermediateList, Tail).

start_task(N, InputList, Task, IntermediateProcFunc) ->
    InputPid = start_tracker(fun () -> input_proc(InputList) end, input),
    IntermediatePid = start_tracker(IntermediateProcFunc, intermediate),
    Emit = fun (KV) -> IntermediatePid ! {emit, KV} end,
    WorkerPids = case Task of
                     {map, Map} ->
                         start_workern(N, fun () -> map_proc(Map, InputPid, Emit) end, mapper);
                     {reduce, Reduce} ->
                         start_workern(N, fun () -> reduce_proc(Reduce, InputPid, Emit) end, reducer)
                 end,
    wait_workers(WorkerPids),
    stop_tracker(InputPid),
    pull_and_stop_tracker(IntermediatePid).

% two types of processes: worker and tracker
% worker: map_proc and reduce_proc, stop after receive 'eof'
% tracker: stop after receive 'stop'

mr_register(Type, Desc, Pid) ->
    register(list_to_atom(lists:flatten(io_lib:format("~p~p: ~p", [Pid, Type, Desc]))), Pid).

% tracker
start_tracker(F, Desc) ->
    Pid = spawn(F),
    mr_register(tracker, Desc, Pid),
    Pid.

stop_tracker(Pid) -> Pid ! stop, ok.

pull_and_stop_tracker(Pid) ->
    Pid ! {pull, self()},
    receive
        Datum ->
            Pid ! stop,
            Datum
    end.

% worker
start_workern(N, F, Desc) ->
    start_workern_iter([], N, F, Desc).

start_workern_iter(Acc, 0, _, _) -> Acc;
start_workern_iter(Acc, N, F, Desc) ->
    Pid = spawn_link(F),
    mr_register(worker, Desc, Pid),
    start_workern_iter([Pid|Acc], N - 1, F, Desc).

wait_workers([]) -> ok;
wait_workers(Pids) ->
    receive
        {'EXIT', Pid, _Why} ->
            wait_workers([X || X <- Pids, X =/= Pid])
    end.

% output proc
output_proc() ->
    output_proc_loop(queue:new()).

output_proc_loop(Queue) ->
    receive
        {emit, KV} ->
            output_proc_loop(queue:in(KV, Queue));
        {pull, From} ->
            From ! queue:to_list(Queue),
            output_proc_loop(Queue);
        stop -> ok
    end.

% groupby proc
groupby_proc() ->
    groupby_proc_loop(dict:new()).

groupby_proc_loop(Bucket) ->
    receive
        {emit, {K, V}} ->
            groupby_proc_loop(dict:append(K, V, Bucket));
        {pull, From} ->
            From ! dict:to_list(Bucket),
            groupby_proc_loop(Bucket);
        stop -> ok
    end.

% input proc
input_proc([]) ->
    receive
        {From, next} ->
            From ! eof,
            input_proc([]);
        stop -> ok
    end;
input_proc([H|T]) ->
    receive
        {From, next} ->
            From ! H,
            input_proc(T);
        stop -> ok
    end.

% map proc
map_proc(Map, InputPid, Emit) ->
    task_proc(InputPid, fun (KV) -> Map(KV, Emit) end).

% reduce proc
reduce_proc(Reduce, InputPid, Emit) ->
    task_proc(InputPid, fun (KV) -> Emit(Reduce(KV)) end).

% task proc
task_proc(InputPid, ProcessAndEmit) ->
    InputPid ! {self(), next},
    receive
        eof -> ok;
        KV ->
            ProcessAndEmit(KV),
            task_proc(InputPid, ProcessAndEmit)
    end.
