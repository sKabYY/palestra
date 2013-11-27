-module(test).
-export([start/1,
         wc/1]).

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
