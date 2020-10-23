-module(task1).

-export([fib/1]).
% error, success
-include("header.hrl").
% calc func
fib_internal(0) -> 0;
fib_internal(1) -> 1;
fib_internal(N) -> fib_internal(N - 1) + fib_internal(N - 2).
% iface func
fib(X) when X < 0 -> {?ERROR,-1};
fib(X) -> {?SUCCESS, fib_internal(X)}.