-module(count_word).
-export([go/1]).

mk_line_input_format_from_file(Lineno, S) ->
    fun () ->
            Line = io:get_line(S, ''),
            case Line of
                eof -> file:close(S), eof;
                _ -> {{Lineno, Line},
                      mk_line_input_format_from_file(Lineno + 1, S)}
            end
    end.

mk_line_input_format(Filename) ->
    {ok, S} = file:open(Filename, read),
    mk_line_input_format_from_file(1, S).

cw_map({_, Line}, Emit) ->
    lists:foreach(
      fun(Char) -> Emit({[Char], 1}) end,
      Line).

cw_reduce({_, Vals}) -> lists:sum(Vals).

go(N) ->
    InputFormat = mk_line_input_format("erlmr.erl"),
    OutputFormat = fun erlmr:default_output_format/1,
    Map = fun cw_map/2,
    Reduce = fun cw_reduce/1,
    erlmr:mapreduce(
      N,
      InputFormat,
      OutputFormat,
      Map,
      Reduce).
