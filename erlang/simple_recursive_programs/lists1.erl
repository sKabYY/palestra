-module(lists1).
-export([min/1, max/1, min_max/1]).

min([E]) -> E;
min([H|T]) ->
    MT = min(T),
    if
        H < MT -> H;
        true -> MT
    end.

max([E]) -> E;
max([H|T]) ->
    MT = max(T),
    if
        H > MT -> H;
        true -> MT
    end.

min_max(L) -> {min(L), max(L)}.
