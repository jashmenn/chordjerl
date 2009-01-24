-module(ch_utils).
-export([max/1]).

% max from http://www.zorched.net/2008/05/28/erlang-example-min-and-max-element-of-a-list/
max([H|T]) ->
    max(H, T).
max(M, []) ->
    M;
max(M, [H|L]) when M > H ->
    max(M, L);
max(_M, [H|L]) ->
    max(H,L).
