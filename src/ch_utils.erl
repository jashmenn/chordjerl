-module(ch_utils).
-export([max/1, list_replace_n/3]).

% max from http://www.zorched.net/2008/05/28/erlang-example-min-and-max-element-of-a-list/
max([H|T]) ->
    max(H, T).
max(M, []) ->
    M;
max(M, [H|L]) when M > H ->
    max(M, L);
max(_M, [H|L]) ->
    max(H,L).


% Replace N in List with NewElement
% 
%  > A = [1,2,3,4,5,6].
%  [1,2,3,4,5,6]
%  > ch_utils:list_replace_n(3, bob, A).
%  [1,2,bob,4,5,6]
%  > ch_utils:list_replace_n(1, gary, A).
%  [gary,2,3,4,5,6]
%
list_replace_n(N, NewElement, List) ->
   {L1, L2} = lists:split(N - 1, List), 
   [_H|T] = L2,
   lists:append([L1, [NewElement], T]).
