-module(test_map).
-export([go/0]).
-import(erlmr, [mrgo/3, mk_input_format_from_list/1]).

map({K, V}, Emit) ->
    Emit({K, V}),
    Emit({K, V * V}).

go() ->
    TestInput = [{a, 1},
                 {b, 2},
                 {c, 3}],
    InputFormat = mk_input_format_from_list(TestInput),
    mrgo(4, InputFormat, fun map/2).
