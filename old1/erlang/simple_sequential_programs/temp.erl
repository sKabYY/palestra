-module(temp).
-export([convert/1, c2f/1, f2c/1]).

c2f(C) -> 32 + 9 * C / 5.
f2c(F) -> 5 * (F - 32) / 9.

convert({c, C}) -> {f, c2f(C)};
convert({f, F}) -> {c, f2c(F)}.
