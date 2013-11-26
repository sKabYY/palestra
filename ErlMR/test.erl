-module(test).
-export([go/0]).

map({K, V}, Emit) ->
    Emit({K, V}),
    Emit({K, V * V}).

reduce({_, ListOfV}) when is_list(ListOfV) ->
    lists:sum(ListOfV).

go() ->
    TestInput = [{a, 1},
                 {b, 2},
                 {c, 3}],
    erlmr:mapreduce(
      4,
      erlmr:mk_input_format_from_list(TestInput),
      fun erlmr:default_output_format/1,
      fun map/2,
      fun reduce/1).
