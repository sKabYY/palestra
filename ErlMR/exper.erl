-module(exper).
-export([start/0,
         exper_m3gzc/3,
         exper_m3gzcmrc/3,
         exper_m3gzcmrp/3]).
-import(m3gzc,
        [loadfile/1,
         difftime/2]).

exper_func(Func, Params, TrainData, TestData, Step) ->
    Len = length(TrainData),
    exper_func_acc([], Func, Params, TrainData, TestData, Step, 0, Len).

exper_func_acc(Acc, Func, Params,
               TrainData, TestData,
               Step, Start, Len) when Start < Len ->
    SubLen = Start + Step,
    SubTrainData = lists:sublist(TrainData, SubLen),
    StartTime = now(),
    L = Func(Params, SubTrainData, TestData),
    EndTime = now(),
    UsedMS = difftime(EndTime, StartTime),
    exper_func_acc([{UsedMS, L}|Acc], Func, Params,
                   TrainData, TestData,
                   Step, SubLen, Len);
exper_func_acc(Acc, _, _, _, _, _, _, _) -> lists:reverse(Acc).

exper_one(Func, Params, Step, OutputFn) ->
    TrainFn = "testdata/traindata.erldat",
    TestFn = "testdata/testdata.erldat",
    TrainData = loadfile(TrainFn),
    TestData = loadfile(TestFn),
    Output = exper_func(Func, Params,
                        TrainData, TestData, Step),
    {ok, S} = file:open(OutputFn, write),
    io:format(S, "~p.~n", [Output]),
    file:close(S).

exper_m3gzc(Lambda, Step, OutputFn) ->
    exper_one(fun m3gzc:m3gzc/3, Lambda, Step, OutputFn).

exper_m3gzcmrc({N, Lambda}, Step, OutputFn) ->
    exper_one(fun m3gzc:m3gzcmrc/3, {N, Lambda}, Step, OutputFn).

exper_m3gzcmrp({N, Lambda}, Step, OutputFn) ->
    exper_one(fun m3gzc:m3gzcmrp/3, {N, Lambda}, Step, OutputFn).

start(N, Step) ->
    Lambda = 0.5,
    Params = {N, Lambda},
    exper_m3gzc(Lambda, Step, "output.m3gzc.erldat"),
    exper_m3gzcmrc(Params, Step, "output.m3gzcmrc.erldat"),
    exper_m3gzcmrp(Params, Step, "output.m3gzcmrp.erldat").
