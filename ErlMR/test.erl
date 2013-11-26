-module(test).
-export([start/1,
         wc/1,
         m3gzc/1]).

% simple test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simple_map({K, V}, Emit) ->
    Emit({K, V}),
    Emit({K, V * V}).

simple_reduce({_, ListOfV}) ->
    lists:sum(ListOfV).

start(N) ->
    TestInput = [{a, 1},
                 {b, 2},
                 {c, 3}],
    mrlib:mapreduce(
      N,
      mrlib:mk_input_format_from_list(TestInput),
      fun mrlib:default_output_format/1,
      fun simple_map/2,
      fun simple_reduce/1).

% word count %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_line_input_format(Filename) ->
    {ok, S} = file:open(Filename, read),
    mk_line_input_format_from_file(1, S).

mk_line_input_format_from_file(Lineno, S) ->
    fun () ->
            case io:get_line(S, '') of
                eof -> file:close(S), eof;
                Line -> {{Lineno, Line},
                      mk_line_input_format_from_file(Lineno + 1, S)}
            end
    end.

wc_map({_, Line}, Emit) ->
    lists:foreach(
      fun(Char) -> Emit({[Char], 1}) end,
      Line).

wc_reduce({_, Vals}) -> lists:sum(Vals).

wc(N) ->
    InputFormat = mk_line_input_format("mrlib.erl"),
    OutputFormat = fun mrlib:default_output_format/1,
    Map = fun wc_map/2,
    Reduce = fun wc_reduce/1,
    mrlib:mapreduce(
      N,
      InputFormat,
      OutputFormat,
      Map,
      Reduce).

% simple m3gzc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%mk_datum_input_format(Filename) ->
%    {ok, S} = file:open(Filename, read),
%    mk_datum_input_format_from_file(1, S).
%
%mk_datum_input_format_from_file(Lineno, S) ->
%    fun () ->
%            case io:read(S, '') of
%                {ok, Datum} -> {{Lineno, Datum},
%                                mk_datum_input_format_from_file(Lineno + 1, S)};
%                eof -> file:close(S), eof
%            end
%    end.

m3_loadfile(Filename) ->
    {ok, S} = file:open(Filename, read),
    Data = m3_loadfile_acc([], S).

m3_loadfile_acc(Acc, S) ->
    case io:read(S, '') of
        {ok, Datum} -> m3_loadfile_acc([Datum|Acc], S);
        eof -> file:close(S), Acc
    end.

m3_mk_pair(A1, A2) ->
    m3_mk_pair1(0, 0, A1, A2, array:size(A1), array:size(A2)).

% Index, Array, Size
m3_mk_pair1(I1, I2, A1, A2, S1, S2) ->
    NextI1 = I1 + 1,
    NextGenerator = if
                        NextI1 == S1 ->
                            m3_mk_pair1(0, I2 + 1,
                                        A1, A2, S1, S2);
                        true ->
                            m3_mk_pair1(NextI1, I2,
                                        A1, A2, S1, S2)
                    end,
    fun () ->
            if
                I2 == S2 -> eof;
                true ->
                    {{{I1, I2},
                      {array:get(I1, A1), array:get(I2, A2)}},
                     NextGenerator}
            end
    end.

gzc(GZCParams, Vp, Vn, Vx) TODO.

m3_gzc_mk_map(GZCParams, Tests)
  fun ({{PosIdx, NegIdx}, {PosVec, NegVec}}, Emit) ->
          lists:foreach(
            fun ({Idx, Vx}) ->
                    Score = gzc(GZCParams, PosVec, NegVec, Vx),
                    Emit({{Idx, PosIdx}, Score})
            end,
            Tests)
  end.

m3gzc(N) ->
    TrainDataPath = "testdata/traindata.erldat",
    TestDataPath = "testdata/testdata.erldat",
    TrainData = m3_loadfile(TrainDataPath),
    TestData = m3_loadfile(TestDataPath),
    {PosDataL, NegDataL} =
        lists:foldl(
          fun ({Label, Vec}, {Ps, Ns}) ->
                  if
                      Label > 0 -> {[Vec|Ps], Ns};
                      true -> {Ps, [Vec|Ns]}
                  end
          end,
          {[], []},
          TrainData),
    PosData = array:from_list(PosDataL),
    NegData = array:from_list(NegDataL),
    InputFormat = m3_mk_pair(PosData, NegData),
    mrlib:mapreduce(
      N,
      InputFormat,
      OutputFormat,
      Map,
      Reduce).
