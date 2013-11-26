-module(test).
-export([start/0,
         wc/1]).

% simple test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
map({K, V}, Emit) ->
    Emit({K, V}),
    Emit({K, V * V}).

reduce({_, ListOfV}) when is_list(ListOfV) ->
    lists:sum(ListOfV).

start() ->
    TestInput = [{a, 1},
                 {b, 2},
                 {c, 3}],
    mrlib:mapreduce(
      4,
      mrlib:mk_input_format_from_list(TestInput),
      fun mrlib:default_output_format/1,
      fun map/2,
      fun reduce/1).

mk_line_input_format_from_file(Lineno, S) ->
    fun () ->
            Line = io:get_line(S, ''),
            case Line of
                eof -> file:close(S), eof;
                _ -> {{Lineno, Line},
                      mk_line_input_format_from_file(Lineno + 1, S)}
            end
    end.

% word count %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_line_input_format(Filename) ->
    {ok, S} = file:open(Filename, read),
    mk_line_input_format_from_file(1, S).

wc_map({_, Line}, Emit) ->
    lists:foreach(
      fun(Char) -> Emit({[Char], 1}) end,
      Line).

wc_reduce({_, Vals}) -> lists:sum(Vals).

wc(N) ->
    InputFormat = mk_line_input_format("mrlib.erl"),
    OutputFormat = fun mrlib:default_output_format/1,
    Map = fun cw_map/2,
    Reduce = fun cw_reduce/1,
    mrlib:mapreduce(
      N,
      InputFormat,
      OutputFormat,
      Map,
      Reduce).
