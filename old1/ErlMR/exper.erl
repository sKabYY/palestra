-module(exper).
-export([exper_normal/2,
         exper_threads/2,
         exper_m3gzc/3,
         exper_m3gzcmrc/3,
         exper_m3gzcmrp/3,
         %%%
         exper_pruning/4,
         exper_twospirals/5,
         exper_m3gzcmp/3,
         exper_m3gzcmpmr/3,
         exper_m3gzcfp/3,
         exper_m3gzcfpmr/3,
         exper_m3gzcap/3,
         exper_m3gzcapmr/3]).
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

exper_prune_func(PruneFunc, PruneParams,
                 PredictFunc, PredictParams,
                 TrainData, TestData, Step) ->
    Len = length(TrainData),
    info("exper: prune: ~p, predict: ~p", [PruneFunc, PredictFunc]),
    info("total #train: ~p", [Len]),
    exper_prune_func_acc([],
                         PruneFunc, PruneParams,
                         PredictFunc, PredictParams,
                         TrainData, TestData, Step, 0, Len).

exper_prune_func_acc(Acc,
                     PruneFunc, PruneParams,
                     PredictFunc, PredictParams,
                     TrainData, TestData,
                     Step, Start, Len) when Start < Len ->
    SubLen = Start + Step,
    SubTrainData = lists:sublist(TrainData, SubLen),
    info("go: #train=~p, #test=~p", [length(SubTrainData), length(TestData)]),
    StartTimePrune = now(),
    M = PruneFunc(PruneParams, SubTrainData),
    EndTimePrune = now(),
    PruneUsedMS = difftime(EndTimePrune, StartTimePrune),
    StartTimePredict = now(),
    L = PredictFunc(PredictParams, M, SubTrainData, TestData),
    EndTimePredict = now(),
    PredictUsedMS = difftime(EndTimePredict, StartTimePredict),
    exper_prune_func_acc([{PruneUsedMS, PredictUsedMS, M, L}|Acc],
                         PruneFunc, PruneParams,
                         PredictFunc, PredictParams,
                         TrainData, TestData,
                         Step, SubLen, Len);
exper_prune_func_acc(Acc, _, _, _, _, _, _ ,_ ,_, _) ->
    lists:reverse(Acc).

exper_prune_one(PruneFunc, PruneParams,
                PredictFunc, PredictParams,
                Step, OutputFn) ->
    TrainFn = "testdata/traindata.erldat",
    TestFn = "testdata/testdata.erldat",
    TrainData = loadfile(TrainFn),
    TestData = loadfile(TestFn),
    Output = exper_prune_func(PruneFunc, PruneParams,
                              PredictFunc, PredictParams,
                              TrainData, TestData, Step),
    savefile(OutputFn, Output).


exper_m3gzcmp({Lambda, Threshold}, Step, OutputFn) ->
    exper_prune_one(
      fun m3gzc:m3gzcmp_prune/2, {Lambda, Threshold},
      fun m3gzc:m3gzcmp_predict/4, Lambda,
      Step, OutputFn).

exper_m3gzcmpmr({N, Lambda, Threshold}, Step, OutputFn) ->
    exper_prune_one(
      fun m3gzc:m3gzcmpmr_prune/2, {N, Lambda, Threshold},
      fun m3gzc:m3gzcmpmr_predict/4, {N, Lambda},
      Step, OutputFn).

exper_m3gzcfp({Lambda, Threshold}, Step, OutputFn) ->
    exper_prune_one(
      fun m3gzc:m3gzcfp_prune/2, {Lambda, Threshold},
      fun m3gzc:m3gzcfp_predict/4, Lambda,
      Step, OutputFn).

exper_m3gzcfpmr({N, Lambda, Threshold}, Step, OutputFn) ->
    exper_prune_one(
      fun m3gzc:m3gzcfpmr_prune/2, {N, Lambda, Threshold},
      fun m3gzc:m3gzcfpmr_predict/4, {N, Lambda},
      Step, OutputFn).

exper_m3gzcap({K, Lambda}, Step, OutputFn) ->
    exper_prune_one(
      fun m3gzc:m3gzcap_prune/2, K,
      fun m3gzc:m3gzcap_predict/4, Lambda,
      Step, OutputFn).

exper_m3gzcapmr({N, K, Lambda}, Step, OutputFn) ->
    exper_prune_one(
      fun m3gzc:m3gzcapmr_prune/2, {N, K},
      fun m3gzc:m3gzcapmr_predict/4, {N, Lambda},
      Step, OutputFn).

exper_pruning(N, K, Threshold, Step) ->
    Lambda = 0.5,
    exper_m3gzcmp({Lambda, Threshold}, Step, "output.m3gzcmp.erldat"),
    exper_m3gzcmpmr({N, Lambda, Threshold}, Step, "output.m3gzcmpmr.erldat"),
    exper_m3gzcfp({Lambda, Threshold}, Step, "output.m3gzcfp.erldat"),
    exper_m3gzcfpmr({N, Lambda, Threshold}, Step, "output.m3gzcfpmr.erldat"),
    exper_m3gzcap({K, Lambda}, Step, "output.m3gzcap.erldat"),
    exper_m3gzcapmr({N, K, Lambda}, Step, "output.m3gzcapmr.erldat").

exper_twospirals(N, K, Threshold, Max, Step) ->
    Lambda = 0.5,
    TrainData = m3gzc:loadfile("testdata/twospirals.erldat"),
    TestData = lists:map(
                 fun ({X, Y}) -> {0, [{1, X}, {2, Y}]} end,
                 meshgrid(Max, Step)),
    exper_ts_m3gzcmrp({N, Lambda, Threshold},
                      TrainData, TestData, "twospirals.m3gzcmrp.dat"),
    exper_ts_m3gzcfpmr({N, Lambda, Threshold},
                       TrainData, TestData, "twospirals.m3gzcfpmr.dat"),
    exper_ts_m3gzcapmr({N, Lambda, K, Threshold},
                       TrainData, TestData, "twospirals.m3gzcapmr.dat").

meshgrid(Max, Step) ->
    Xs = linspace(-Max, Max, Step),
    Ys = linspace(-Max, Max, Step),
    mkpair(Xs, Ys).

linspace(Min, Max, Step) ->
    linspace_acc([], Min, Max, Step).

linspace_acc(Acc, Cur, Max, _) when Cur > Max -> lists:reverse(Acc);
linspace_acc(Acc, Cur, Max, Step) ->
    linspace_acc([Cur|Acc], Cur + Step, Max, Step).

mkpair(L1, L2) ->
    mkpair_acc([], L1, L2, L1).

mkpair_acc(Acc, [], [_|T2], L1) ->
    mkpair_acc(Acc, L1, T2, L1);
mkpair_acc(Acc, _, [], _) ->
    lists:reverse(Acc);
mkpair_acc(Acc, [H1|T1], L2, L1) ->
    [H2|_] = L2,
    mkpair_acc([{H1, H2}|Acc], T1, L2, L1).

exper_ts_save(OutputFn, Threshold, TestData, Output) ->
    Data = lists:zip(
             lists:map(
               fun ({_, [{1, X}, {2, Y}]}) -> {X, Y} end,
               TestData),
             lists:map(
               fun (Score) ->
                       if
                           Score > Threshold -> 1;
                           Score < -Threshold -> -1;
                           true -> 0
                       end
               end,
               Output)),
    {ok, S} = file:open(OutputFn, write),
    lists:foreach(
      fun ({{X, Y}, Z}) ->
              io:format(S, "~p\t~p\t~p\n", [X, Y, Z])
      end,
      Data),
    file:close(S).

exper_ts_m3gzcmrp({N, Lambda, Threshold}, TrainData, TestData, OutputFn) ->
    {_, Output} = m3gzc:m3gzcmrp({N, Lambda}, TrainData, TestData),
    exper_ts_save(OutputFn, Threshold, TestData, Output).

exper_ts_m3gzcfpmr({N, Lambda, Threshold}, TrainData, TestData, OutputFn) ->
    Modules = m3gzc:m3gzcfpmr_prune({N, Lambda, Threshold}, TrainData),
    {_, Output} = m3gzc:m3gzcfpmr_predict({N, Lambda}, Modules,
                                          TrainData, TestData),
    exper_ts_save(OutputFn, Threshold, TestData, Output).

exper_ts_m3gzcapmr({N, Lambda, K, Threshold}, TrainData, TestData, OutputFn) ->
    Modules = m3gzc:m3gzcapmr_prune({N, K}, TrainData),
    {_, Output} = m3gzc:m3gzcapmr_predict({N, Lambda}, Modules,
                                          TrainData, TestData),
    exper_ts_save(OutputFn, Threshold, TestData, Output).
