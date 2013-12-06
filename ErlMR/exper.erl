-module(exper).
-export([exper_normal/2,
         exper_threads/2,
         exper_m3gzc/3,
         exper_m3gzcmrc/3,
         exper_m3gzcmrp/3]).
-import(mrlib,
        [info/2]).
-import(m3gzc,
        [loadfile/1,
         savefile/2,
         difftime/2]).

exper_func(Func, Params, TrainData, TestData, Step) ->
    Len = length(TrainData),
    info("exper: ~p", [Func]),
    info("total #train: ~p", [Len]),
    exper_func_acc([], Func, Params, TrainData, TestData, Step, 0, Len).

exper_func_acc(Acc, Func, Params,
               TrainData, TestData,
               Step, Start, Len) when Start < Len ->
    SubLen = Start + Step,
    SubTrainData = lists:sublist(TrainData, SubLen),
    info("go: #train=~p, #test=~p", [length(SubTrainData), length(TestData)]),
    StartTime = now(),
    L = Func(Params, SubTrainData, TestData),
    EndTime = now(),
    info("done!", []),
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
    savefile(OutputFn, Output).

exper_m3gzc(Lambda, Step, OutputFn) ->
    exper_one(fun m3gzc:m3gzc/3, Lambda, Step, OutputFn).

exper_m3gzcmrc({N, Lambda}, Step, OutputFn) ->
    exper_one(fun m3gzc:m3gzcmrc/3, {N, Lambda}, Step, OutputFn).

exper_m3gzcmrp({N, Lambda}, Step, OutputFn) ->
    exper_one(fun m3gzc:m3gzcmrp/3, {N, Lambda}, Step, OutputFn).

exper_normal(N, Step) ->
    Lambda = 0.5,
    Params = {N, Lambda},
    exper_m3gzc(Lambda, Step, "output.m3gzc.erldat"),
    exper_m3gzcmrc(Params, Step, "output.m3gzcmrc.erldat"),
    exper_m3gzcmrp(Params, Step, "output.m3gzcmrp.erldat").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exper_th_one(Func, CN, DN, N, Lambda, TrainData, TestData)
  when CN < N ->
    NextN = DN + CN,
    StartTime = now(),
    Func({NextN, Lambda}, TrainData, TestData),
    EndTime = now(),
    UsedMS = difftime(EndTime, StartTime),
    Rest = exper_th_one(Func, NextN, DN, N, Lambda, TrainData, TestData),
    [{NextN, UsedMS}|Rest];
exper_th_one(_, _, _, _, _, _, _) -> [].


exper_threads(DN, N) ->
    TrainFn = "testdata/traindata.erldat",
    TestFn = "testdata/testdata.erldat",
    TrainData = loadfile(TrainFn),
    TestData = loadfile(TestFn),
    Lambda = 0.5,
    MRCT = exper_th_one(fun m3gzc:m3gzcmrc/3, 0, DN, N,
                        Lambda, TrainData, TestData),
    MRPT = exper_th_one(fun m3gzc:m3gzcmrp/3, 0, DN, N,
                        Lambda, TrainData, TestData),
    savefile("mrc_threads.erldat", MRCT),
    savefile("mrp_threads.erldat", MRPT),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
