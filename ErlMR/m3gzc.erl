-module(m3gzc).
-export([loadfile/1,
         savefile/2,
         difftime/2,
         traindata_info/1,
         test_pf/0,
         m3gzc_prune_factor/2,
         m3gzc/3,
         m3gzcmrc/3,
         m3gzcmrp/3,
         count_mp_modules/1,
         m3gzcmp_prune/2,
         m3gzcmp_predict/4,
         m3gzcmpmr_prune/2,
         m3gzcmpmr_predict/4,
         m3gzcfp_prune/2,
         m3gzcfp_predict/4,
         m3gzcfpmr_prune/2,
         m3gzcfpmr_predict/4]).
-import(mrlib,
        [mapreduce/3,
         info/2]).

% input of m3gzc: Lambda, TrainData, TestData

% m3gzclib %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loadfile(Filename) ->
    {ok, S} = file:open(Filename, read),
    {ok, Data} = io:read(S, ''),
    file:close(S),
    Data.

savefile(Filename, Data) ->
    {ok, S} = file:open(Filename, write),
    io:format(S, "~p.~n", [Data]),
    file:close(S).

difftime({MeS1, S1, MiS1}, {MeS2, S2, MiS2}) ->
    DMeS = MeS1 - MeS2,
    DS = S1 - S2,
    DMiS = MiS1 - MiS2,
    (1000000 * DMeS + DS) * 1000 + DMiS / 1000.

%dotproduct(V1, V2) -> dotproduct_acc(0, V1, V2).
%
%dotproduct_acc(Acc, [], _) -> Acc;
%dotproduct_acc(Acc, _, []) -> Acc;
%dotproduct_acc(Acc, [A1|V1], [A2|V2]) ->
%    {D1, X1} = A1,
%    {D2, X2} = A2,
%    if
%        D1 > D2 -> dotproduct_acc(Acc, [A1|V1], V2);
%        D2 < D1 -> dotproduct_acc(Acc, V1, [A2|V2]);
%        true -> dotproduct_acc(Acc + X1 * X2, V1, V2)
%    end.

squarelength(V) -> squarelength_acc(0, V).

squarelength_acc(Acc, []) -> Acc;
squarelength_acc(Acc, [{_, X}|V]) ->
    squarelength_acc(Acc + X * X, V).

diffv(V1, V2) -> diffv_acc([], V1, V2).

diffv_acc(Acc, [], []) -> lists:reverse(Acc);
diffv_acc(Acc, V1, []) -> lists:revers(V1) ++ Acc;
diffv_acc(Acc, [], V2) ->
    lists:revers(lists:map(fun ({D, X}) -> {D, -X} end, V2)) ++ Acc;
diffv_acc(Acc, [A1|V1], [A2|V2]) ->
    {D1, X1} = A1,
    {D2, X2} = A2,
    if
        D1 > D2 -> diffv_acc([{D2, -X2}|Acc], [A1|V1], V2);
        D2 < D1 -> diffv_acc([A1|Acc], V1, [A2|V2]);
        true -> diffv_acc([{D1, X1 - X2}|Acc], V1, V2)
    end.


squaredistance(V1, V2) ->
    squarelength(diffv(V1, V2)).

% gzc = exp(-(|Vp-Vx|/|Vp-Vn|/lam)^2) - exp(-(|Vn-Vx|/|Vp-Vn|/lam)^2)
% |Vp-Vx|^2 = |Vp|^2 + |Vx|^2 - 2 * dot(Vp, Vx)
% |Vn-Vx|^2 = |Vn|^2 + |Vx|^2 - 2 * dot(Vn, Vx)
% |Vp-Vn|^2 = |Vp|^2 + |Vn|^2 - 2 * dot(Vp, Vn)
%gzc(Lambda, Vp, Vn, Vx) ->
%    SLVp = squarelength(Vp),
%    SLVn = squarelength(Vn),
%    SLVx = squarelength(Vx),
%    VpVx = dotproduct(Vp, Vx),
%    VnVx = dotproduct(Vn, Vx),
%    VpVn = dotproduct(Vp, Vn),
%    VpVx2 = SLVp + SLVx - 2 * VpVx,
%    VnVx2 = SLVn + SLVx - 2 * VnVx,
%    VpVn2 = SLVp + SLVn - 2 * VpVn,
%    Sigma = Lambda * Lambda * VpVn2,
%    math:exp(-VpVx2 / Sigma) - math:exp(-VnVx2 / Sigma).

gzc(Lambda, Vp, Vn, Vx) ->
    VpVx2 = squaredistance(Vp, Vx),
    VnVx2 = squaredistance(Vn, Vx),
    VpVn2 = squaredistance(Vp, Vn),
    Sigma2 = Lambda * Lambda * VpVn2,
    math:exp(-VpVx2 / Sigma2) - math:exp(-VnVx2 / Sigma2).

unzip_fst(L) ->
    lists:map(fun ({I, _}) -> I end, L).

unzip_snd(L) ->
    lists:map(fun ({_, V}) -> V end, L).

% index starts from 1
extidx(Vecs) -> lists:zip(lists:seq(1, length(Vecs)), Vecs).

label_partition(Data) ->
    {PosData, NegData} =
        lists:partition(
          fun ({Label, _}) -> Label > 0 end,
          Data),
    PosVecs = unzip_snd(PosData),
    NegVecs = unzip_snd(NegData),
    {PosVecs, NegVecs}.

traindata_info(TrainData) ->
    {PosData, NegData} =
        lists:partition(
          fun ({Label, _}) -> Label > 0 end,
          TrainData),
    Np = length(PosData),
    Nn = length(NegData),
    {Np, Nn, Np * Nn}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m3gzc_func(M3gzcFunc, Params, TrainData, TestData) ->
    {PosVecs, NegVecs} = label_partition(TrainData),
    {TestLabels, TestVecs} = lists:unzip(TestData),
    PredictLabels = M3gzcFunc(Params, PosVecs, NegVecs, TestVecs),
    {TestLabels, PredictLabels}.

% M3-GZC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m3gzc(Lambda, TrainData, TestData) ->
    m3gzc_func(fun m3gzc1/4, Lambda, TrainData, TestData).

m3gzc1(Lambda, PosVecs, NegVecs, TestVecs) ->
    lists:map(
      fun (TVec) ->
              m3gzc_max(Lambda, PosVecs, NegVecs, TVec)
      end,
      TestVecs).

m3gzc_max(Lambda, PVecs, NVecs, TVec) ->
    lists:max(lists:map(
                fun (PVec) ->
                        m3gzc_min(Lambda, PVec, NVecs, TVec)
                end,
                PVecs)).
m3gzc_min(Lambda, PVec, NVecs, TVec) ->
    lists:min(lists:map(
                fun (NVec) ->
                        gzc(Lambda, PVec, NVec, TVec)
                end,
                NVecs)).


% M3-GZC-MRC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m3gzcmrc({N, Lambda}, TrainData, TestData) ->
    m3gzc_func(fun m3gzcmrc1/4, {N, Lambda}, TrainData, TestData).

m3gzcmrc1({N, Lambda}, PosVecs, NegVecs, TestVecs) ->
    PosVecArr = array:from_list(PosVecs),
    NegVecArr = array:from_list(NegVecs),
    info("mkpair", []),
    InputList = m3gzcmrc_mk_pair(PosVecArr, NegVecArr),
    info("start mapreduce", []),
    Output = mapreduce(
               N,
               InputList,
               [{map, m3gzcmrc_mk_map(Lambda, extidx(TestVecs))},
                {reduce, fun m3gzcmrc_min_reduce/1},
                {reduce, fun m3gzcmrc_max_reduce/1}]),
    unzip_snd(lists:keysort(1, Output)).

m3gzcmrc_mk_pair(A1, A2) ->
    m3gzcmrc_mk_pair_acc(
      [], 0, 0, A1, A2, array:size(A1), array:size(A2)).

% Index, Array, Size
m3gzcmrc_mk_pair_acc(Acc, I1, I2, A1, A2, S1, S2) ->
    if
        I2 == S2 -> lists:reverse(Acc);
        true -> NextI1 = I1 + 1,
                Pair = {{I1, I2}, {array:get(I1, A1), array:get(I2, A2)}},
                NewAcc = [Pair|Acc],
                if
                    NextI1 == S1 ->
                        m3gzcmrc_mk_pair_acc(NewAcc,
                                            0, I2 + 1,
                                            A1, A2, S1, S2);
                    true ->
                        m3gzcmrc_mk_pair_acc(NewAcc,
                                            NextI1, I2,
                                            A1, A2, S1, S2)
                end
    end.

m3gzcmrc_mk_map(Lambda, TestVecs) ->
    fun ({{PosIdx, _}, {PosVec, NegVec}}, Emit) ->
            lists:foreach(
              fun ({Idx, Vx}) ->
                      Score = gzc(Lambda, PosVec, NegVec, Vx),
                      Emit({{Idx, PosIdx}, Score})
              end,
              TestVecs)
    end.

m3gzcmrc_min_reduce({{Idx, _}, ListOfScore}) ->
    {Idx, lists:min(ListOfScore)}.

m3gzcmrc_max_reduce({Idx, ListOfScore}) ->
    {Idx, lists:max(ListOfScore)}.

% M3-GZC-MRP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m3gzcmrp({N, Lambda}, TrainData, TestData) ->
    m3gzc_func(fun m3gzcmrp1/4, {N, Lambda}, TrainData, TestData).

m3gzcmrp1 ({N, Lambda}, PosVecs, NegVecs, TestVecs) ->
    InputList = extidx(PosVecs),
    info("start mapreduce", []),
    Output = mapreduce(
               N,
               InputList,
               [{map, m3gzcmrp_mk_map(Lambda,
                                      NegVecs,
                                      extidx(TestVecs))},
                {reduce, fun m3gzcmrp_reduce/1}]),
    unzip_snd(lists:keysort(1, Output)).

m3gzcmrp_mk_map(Lambda, NegVecs, TestVecs) ->
    fun ({_, Vp}, Emit) ->
            lists:foreach(
              fun ({Idx, Vx}) ->
                      Score = lists:min(
                                lists:map(
                                  fun (Vn) ->
                                          gzc(Lambda, Vp, Vn, Vx)
                                  end,
                                  NegVecs)),
                      Emit({Idx, Score})
              end,
              TestVecs)
    end.

m3gzcmrp_reduce(KV) -> m3gzcmrc_max_reduce(KV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Func(x) = 0
root(Func, Eps) ->
    B = find_first_less(Func, 1, 1 / Eps),
    root_range(Func, Eps, 0, B).

find_first_less(Func, B, Max) ->
    Y = Func(B),
    if
        Y > 0 ->
            find_first_less(Func, 2 * B, Max);
        B > Max -> throw(overflow);
        true -> B
    end.

root_range(Func, Eps, A, B) ->
    C = (A + B) / 2,
    if
        B - A >= Eps ->
            Y = Func(C),
            if
                Y > 0 -> root_range(Func, Eps, C, B);
                Y < 0-> root_range(Func, Eps, A, C);
                true -> C
            end;
        true -> C
    end.

exp_ms(X) -> math:exp(-X*X).

mk_k1equation(Lambda, Threshold) ->
    fun (K1) ->
            exp_ms(K1 / Lambda) - exp_ms((1 + K1) / Lambda) - Threshold
    end.

mk_k2equation(Lambda, Threshold) ->
    fun (K2) ->
            exp_ms(K2 / Lambda) - exp_ms((1 - K2) / Lambda) - Threshold
    end.

m3gzc_prune_factor(Lambda, Threshold) ->
    Eps = 0.000001,
    K1 = root(mk_k1equation(Lambda, Threshold), Eps),
    K2 = root(mk_k2equation(Lambda, Threshold), Eps),
    R = K1 / K2,
    R * R.

test_pf() ->
    N = 100,
    Xs = lists:map(
           fun (X) -> X / N end,
           lists:seq(0, N)),
    Ys = lists:map(
           fun (X) -> m3gzc_prune_factor(0.5, X) end,
           Xs),
    savefile("test_pf.erldat", lists:zip(Xs, Ys)).

% M3-GZC-MP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count_mp_modules(Modules) ->
    lists:sum(lists:map(
                fun ({_, List}) -> length(List) end,
                Modules)).

m3gzc_prune_func(Func, Params, Lambda, Threshold, TrainData) ->
    {PosVecs, NegVecs} = label_partition(TrainData),
    KK = m3gzc_prune_factor(Lambda, Threshold),
    Func(Params, KK, PosVecs, NegVecs).

% M3-GZC-MP prune

m3gzcmp_prune({Lambda, Threshold}, TrainData) ->
    m3gzc_prune_func(fun m3gzcmp_prune1/4, {},
                     Lambda, Threshold, TrainData).

m3gzcmp_prune1({}, KK, PosVecs, NegVecs) ->
    NegPLs = m3gzcmp_pls(KK, PosVecs, NegVecs),
    PosPLs = m3gzcmp_pls(KK, NegVecs, PosVecs),
    Ms = lists:map(
           fun ({PVec, PPL}) ->
                   m3gzcmp_modules_vec(PVec, PPL, NegVecs, NegPLs)
           end,
           lists:zip(PosVecs, PosPLs)),
    extidx(Ms).

m3gzcmp_pls(KK, IterVecs, TargetVecs) ->
    lists:map(
      fun (V) ->
              lists:min(lists:map(
                          fun (IV) ->
                                  squaredistance(V, IV)
                          end,
                          IterVecs)) * KK
      end,
      TargetVecs).

m3gzcmp_modules_vec(PVec, PPL, NegVecs, NegPLs) ->
    unzip_fst(
      lists:filter(
        fun ({_, {NVec, NPL}}) ->
                L = squaredistance(PVec, NVec),
                L < PPL orelse L < NPL
        end,
        extidx(lists:zip(NegVecs, NegPLs)))).

% M3-GZC-MP predict

m3gzcmp_predict(Lambda, Modules, TrainData, TestData) ->
    m3gzc_func(fun m3gzcmp_predict1/4, {Lambda, Modules}, TrainData, TestData).

m3gzcmp_predict1({Lambda, Modules}, PosVecs, NegVecs, TestVecs) ->
    lists:map(
      fun (TVec) ->
              m3gzcmp_predict1_one({Lambda, Modules}, PosVecs, NegVecs, TVec)
      end,
      TestVecs).

m3gzcmp_predict1_one({Lambda, Modules}, PosVecs, NegVecs, TVec) ->
    NegLen = length(NegVecs),
    NegVecsArr = array:from_list(NegVecs),
    NegBuf = array:new(NegLen, {default, inf}),
    m3gzcmp_predict1_one_iter(0, NegBuf, {Lambda, Modules}, PosVecs, NegVecsArr, TVec).

m3gzcmp_predict1_one_iter(PScore, NegBuf, {_, []}, [], _, _) ->
    PScore - lists:max(array:to_list(NegBuf));
m3gzcmp_predict1_one_iter(PScore, NegBuf,
                          {Lambda, [{_, M}|Modules]}, [PVec|PosVecs], NegVecsArr, TVec) ->
    NegSs = m3gzcmp_score_line(Lambda, M, PVec, NegVecsArr, TVec),
    PS = lists:min(NegSs),
    NewNegBuf = m3gzcmp_update_negbuf(M, NegSs, NegBuf),
    m3gzcmp_predict1_one_iter(
      max(PScore, PS), NewNegBuf,
      {Lambda, Modules}, PosVecs, NegVecsArr, TVec).

m3gzcmp_score_line(Lambda, M, PVec, NegVecsArr, TVec) ->
    lists:map(
      fun (NIdx) ->
              NVec = array:get(NIdx - 1, NegVecsArr),
              gzc(Lambda, PVec, NVec, TVec)
      end,
      M).

m3gzcmp_update_negbuf([], [], NegBuf) -> NegBuf;
m3gzcmp_update_negbuf([Mod|M], [NS|NegSs], NegBuf) ->
    NSold = array:get(Mod - 1, NegBuf),
    NewNegBuf = if
                    -NS < NSold -> array:set(Mod - 1, -NS, NegBuf);
                    true -> NegBuf
                end,
    m3gzcmp_update_negbuf(M, NegSs, NewNegBuf).

% M3-GZC-MP-MR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% M3-GZC-MP-MR prune

m3gzcmpmr_prune({N, Lambda, Threshold}, TrainData) ->
    m3gzc_prune_func(fun m3gzcmpmr_prune1/4, N, Lambda, Threshold, TrainData).

m3gzcmpmr_prune1(N, KK, PosVecs, NegVecs) ->
    NegPLs = m3gzcmpmr_pls(N, KK, PosVecs, NegVecs),
    PosPLs = m3gzcmpmr_pls(N, KK, NegVecs, PosVecs),
    Output = mapreduce(
               N,
               extidx(lists:zip(PosVecs, PosPLs)),
               [{reduce, m3gzcmpmr_prune_mk_reduce(KK, NegVecs, NegPLs)}]),
    lists:keysort(1, Output).

m3gzcmpmr_prune_mk_reduce(KK, NegVecs, NegPLs) ->
    fun ({Idx, {PVec, PPL}}) ->
            NIdces = m3gzcmpmr_prune_reduce_acc([], 1, PVec, PPL, KK, NegVecs, NegPLs),
            {Idx, NIdces}
    end.

m3gzcmpmr_prune_reduce_acc(Acc, _, _, _, _, [], []) -> lists:reverse(Acc);
m3gzcmpmr_prune_reduce_acc(Acc, NIdx, PVec, PPL, KK, [NVec|NVecs], [NPL|NPLs]) ->
    SD = squaredistance(PVec, NVec),
    if
        SD >= PPL andalso SD >= NPL ->
            m3gzcmpmr_prune_reduce_acc(Acc, NIdx + 1, PVec, PPL, KK, NVecs, NPLs);
        true ->
            m3gzcmpmr_prune_reduce_acc([NIdx|Acc], NIdx + 1, PVec, PPL, KK, NVecs, NPLs)
    end.

m3gzcmpmr_pls(N, KK, IterVecs, TargetVecs) ->
    Output = mapreduce(
               N,
               extidx(TargetVecs),
               [{reduce, m3gzcmpmr_pls_mk_reduce(KK, IterVecs)}]),
    unzip_snd(lists:keysort(1, Output)).

m3gzcmpmr_pls_mk_reduce(KK, IterVecs) ->
    fun ({Idx, Vec}) ->
            MinL = lists:min(lists:map(
                             fun (IVec) ->
                                     squaredistance(IVec, Vec)
                             end,
                             IterVecs)),
            {Idx, KK * MinL}
    end.

% M3-GZC-MP-MR predict

m3gzcmpmr_predict({N, Lambda}, Modules, TrainData, TestData) ->
    m3gzc_func(fun m3gzcmpmr_predict1/4, {N, Lambda, Modules}, TrainData, TestData).

m3gzcmpmr_predict1({N, Lambda, Modules}, PosVecs, NegVecs, TestVecs) ->
    InputList = lists:map(
                  fun ({{PIdx, Ns}, PVec}) -> {PIdx, {PVec, Ns}} end,
                  lists:zip(Modules, PosVecs)),
    NegVecsArr = array:from_list(NegVecs),
    Output = mapreduce(
               N,
               InputList,
               [{map, m3gzcmpmr_predict_mk_map(Lambda, NegVecsArr, extidx(TestVecs))},
                {reduce, fun m3gzcmpmr_predict_reduce/1},
                {reduce, fun m3gzcmpmr_predict_reduce_min/1},
                {reduce, fun m3gzcmpmr_predict_reduce_merge/1}]),
    unzip_snd(lists:keysort(1, Output)).

m3gzcmpmr_predict_mk_map(Lambda, NVA, TestVecs) ->
    fun ({_, {PVec, Ns}}, Emit) ->
            lists:foreach(
              fun ({Idx, TVec}) ->
                      ScoreList = mp_list(Lambda, TVec, PVec, Ns, NVA),
                      lists:foreach(
                        fun ({NIdx, Score}) -> Emit({{neg, Idx, NIdx}, Score}) end,
                        ScoreList),
                      Emit({{pos, Idx}, lists:min(unzip_snd(ScoreList))})
              end,
              TestVecs)
    end.

mp_list(Lambda, TVec, PVec, Ns, NVA) ->
    lists:map(
      fun (I) ->
              NVec = array:get(I - 1, NVA),
              {I, gzc(Lambda, PVec, NVec, TVec)}
      end,
      Ns).

m3gzcmpmr_predict_reduce({{pos, Idx}, ScoreList}) ->
    {Idx, max(0, lists:max(ScoreList))};
m3gzcmpmr_predict_reduce({{neg, Idx, _}, ScoreList}) ->
    {{neg, Idx}, lists:max(ScoreList)}.

m3gzcmpmr_predict_reduce_min({Idx, [Score]}) ->
    {Idx, Score};
m3gzcmpmr_predict_reduce_min({{neg, Idx}, ScoreList}) ->
    {Idx, min(0, lists:min(ScoreList))}.

m3gzcmpmr_predict_reduce_merge({Idx, [S1, S2]}) ->
    {Idx, S1 + S2}.

% M3-GZC-FP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% M3-GZC-FP prune

m3gzcfp_prune({Lambda, Threshold}, TrainData) ->
    m3gzc_prune_func(fun m3gzcfp_prune1/4, {},
                     Lambda, Threshold, TrainData).

m3gzcfp_prune1({}, KK, PosVecs, NegVecs) ->
    PosMods = m3gzcfp_prune_single(KK, PosVecs, NegVecs),
    NegMods = m3gzcfp_prune_single(KK, NegVecs, PosVecs),
    {extidx(PosMods), extidx(NegMods)}.

m3gzcfp_prune_single(KK, PosVecs, NegVecs) ->
    lists:map(
      fun (PVec) ->
              m3gzcfp_prune_single_one(KK, PVec, NegVecs)
      end,
      PosVecs).

m3gzcfp_prune_single_one(KK, PVec, NegVecs) ->
    SquareDists = lists:map(
                    fun (NVec) ->
                            squaredistance(PVec, NVec)
                    end,
                    NegVecs),
    PruneLength = KK * lists:min(SquareDists),
    unzip_fst(
      lists:filter(
        fun ({_, SD}) ->
                SD < PruneLength
        end,
        extidx(SquareDists))).

% M3-GZ-FP predict

m3gzcfp_predict(Lambda, Modules, TrainData, TestData) ->
    m3gzc_func(fun m3gzcfp_predict1/4, {Lambda, Modules}, TrainData, TestData).

m3gzcfp_predict1({Lambda, Modules}, PosVecs, NegVecs, TestVecs) ->
    lists:map(
      fun (TVec) ->
              m3gzcfp_predict1_one({Lambda, Modules}, PosVecs, NegVecs, TVec)
      end,
      TestVecs).

m3gzcfp_predict1_one({Lambda, {PosModules, NegModules}},
                     PosVecs, NegVecs, TVec) ->
    PScore = m3gzcfp_predict_single(
               Lambda, PosModules, PosVecs, NegVecs, TVec),
    NScore = m3gzcfp_predict_single(
               Lambda, NegModules, NegVecs, PosVecs, TVec),
    PScore - NScore.

m3gzcfp_predict_single(Lambda, Modules, PosVecs, NegVecs, TVec) ->
    NegVecsArr = array:from_list(NegVecs),
    lists:max(
      lists:map(
        fun ({PVec, NegMods}) ->
                lists:min(
                  lists:map(
                    fun (NIdx) ->
                            NVec = array:get(NIdx - 1, NegVecsArr),
                            gzc(Lambda, PVec, NVec, TVec)
                    end,
                    NegMods))
        end,
        lists:zip(PosVecs, unzip_snd(Modules)))).

% M3-GZC-FP-MR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% M3-GZC-FP-MR prune

m3gzcfpmr_prune({N, Lambda, Threshold}, TrainData) ->
    m3gzc_prune_func(fun m3gzcfpmr_prune1/4, N, Lambda, Threshold, TrainData).

m3gzcfpmr_prune1(N, KK, PosVecs, NegVecs) ->
    PosMods = m3gzcfpmr_prune_single(N, KK, PosVecs, NegVecs),
    NegMods = m3gzcfpmr_prune_single(N, KK, NegVecs, PosVecs),
    {PosMods, NegMods}.

m3gzcfpmr_prune_single(N, KK, PosVecs, NegVecs) ->
    Output = mapreduce(
               N,
               extidx(PosVecs),
               [{reduce, m3gzcfpmr_prune_mk_reduce(KK, NegVecs)}]),
    lists:keysort(1, Output).

m3gzcfpmr_prune_mk_reduce(KK, NegVecs) ->
    fun ({PIdx, PVec}) ->
            {PIdx, m3gzcfp_prune_single_one(KK, PVec, NegVecs)}
    end.


% M3-GZC-FP-MR predict

m3gzcfpmr_predict({N, Lambda}, Modules, TrainData, TestData) ->
    m3gzc_func(fun m3gzcfpmr_predict1/4, {N, Lambda, Modules},
               TrainData, TestData).

m3gzcfpmr_predict1({N, Lambda, {PosModules, NegModules}},
                   PosVecs, NegVecs, TestVecs) ->
    PosScores = m3gzcfpmr_predict_single(
                  {N, Lambda, PosModules}, PosVecs, NegVecs, TestVecs),
    NegScores = m3gzcfpmr_predict_single(
                  {N, Lambda, NegModules}, NegVecs, PosVecs, TestVecs),
    lists:map(
      fun ({PS, NS}) -> PS - NS end,
      lists:zip(PosScores, NegScores)).

m3gzcfpmr_predict_single({N, Lambda, Modules},
                         PosVecs, NegVecs, TestVecs) ->
    NegVecsArr = array:from_list(NegVecs),
    Output = mapreduce(
               N,
               m3gzcfpmr_predict_mk_input(PosVecs, Modules),
               [{map, m3gzcfpmr_predict_mk_map(Lambda,
                                               NegVecsArr,
                                               extidx(TestVecs))},
                {reduce, fun m3gzcfpmr_predict_reduce/1}]),
    unzip_snd(lists:keysort(1, Output)).

m3gzcfpmr_predict_mk_input(PosVecs, Modules) ->
    lists:map(
      fun ({PVec, {PIdx, NMods}}) -> {PIdx, {PVec, NMods}} end,
      lists:zip(PosVecs, Modules)).

m3gzcfpmr_predict_mk_map(Lambda, NegVecsArr, TestVecs) ->
    fun ({_, {PVec, NMods}}, Emit) ->
            lists:foreach(
              fun ({TIdx, TVec}) ->
                      Score = lists:min(
                                lists:map(
                                  fun (NIdx) ->
                                          NVec = array:get(NIdx - 1, NegVecsArr),
                                          gzc(Lambda, PVec, NVec, TVec)
                                  end,
                                  NMods)),
                      Emit({TIdx, Score})
              end,
              TestVecs)
    end.

m3gzcfpmr_predict_reduce({TIdx, ScoreList}) ->
    {TIdx, lists:max(ScoreList)}.
