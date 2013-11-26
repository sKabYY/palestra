-module(test).
-export([go/0]).
-import(erlmr, [mrgo/5, mk_input_format_from_list/1]).

map({K, V}, Emit) ->
    Emit({K, V}),
    Emit({K, V * V}).

output_format({K, V}) ->
    io:format("~p => ~p~n", [K, V]),
    fun output_format/1.

reduce({_, ListOfV}) when is_list(ListOfV) ->
    lists:sum(ListOfV).

go() ->
    TestInput = [{a, 1},
                 {b, 2},
                 {c, 3}],
    InputFormat = mk_input_format_from_list(TestInput),
    mrgo(4,
         InputFormat,
         fun output_format/1,
         fun map/2,
         fun reduce/1).
