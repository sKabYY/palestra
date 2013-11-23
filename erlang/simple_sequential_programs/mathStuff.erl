-module(mathStuff).
-export([perimeter/1]).

perimeter({square, Side}) -> 4 * Side;
perimeter({circle, Radius}) -> Radius * 2 * 3.1415;
perimeter({triangle, A, B, C}) -> A + B + C.
