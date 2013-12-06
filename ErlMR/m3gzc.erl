-module(m3gzc).
-export([loadfile/1,
         savefile/2,
         difftime/2,
         m3gzc/3,
         m3gzcmrc/3,
         m3gzcmrp/3]).
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

dotproduct(V1, V2) -> dotproduct_acc(0, V1, V2).

dotproduct_acc(Acc, [], _) -> Acc;
dotproduct_acc(Acc, _, []) -> Acc;
dotproduct_acc(Acc, [A1|V1], [A2|V2]) ->
    {D1, X1} = A1,
    {D2, X2} = A2,
    if
        D1 > D2 -> dotproduct_acc(Acc, [A1|V1], V2);
        D2 < D1 -> dotproduct_acc(Acc, V1, [A2|V2]);
        true -> dotproduct_acc(Acc + X1 * X2, V1, V2)
    end.

squarelength(V) -> dotproduct(V, V).

% gzc = exp(-(|Vp-Vx|/|Vp-Vn|/lam)^2) - exp(-(|Vn-Vx|/|Vp-Vn|/lam)^2)
% |Vp-Vx|^2 = |Vp|^2 + |Vx|^2 - 2 * dot(Vp, Vx)
% |Vn-Vx|^2 = |Vn|^2 + |Vx|^2 - 2 * dot(Vn, Vx)
% |Vp-Vn|^2 = |Vp|^2 + |Vn|^2 - 2 * dot(Vp, Vn)
gzc(Lambda, Vp, Vn, Vx) ->
    SLVp = squarelength(Vp),
    SLVn = squarelength(Vn),
    SLVx = squarelength(Vx),
    VpVx = dotproduct(Vp, Vx),
    VnVx = dotproduct(Vn, Vx),
    VpVn = dotproduct(Vp, Vn),
    VpVx2 = SLVp + SLVx - 2 * VpVx,
    VnVx2 = SLVn + SLVx - 2 * VnVx,
    VpVn2 = SLVp + SLVn - 2 * VpVn,
    Sigma = Lambda * Lambda * VpVn2,
    math:exp(-VpVx2 / Sigma) - math:exp(-VnVx2 / Sigma).

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
