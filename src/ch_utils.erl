-module(ch_utils).
-export([max/1, list_replace_n/3, partition_integer/2, each_with_index/2]).
-compile(export_all).

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


partition_integer(N,P) ->
    partition_integer(lists:reverse(integer_to_list(N)),P,[]).

partition_integer([A,B,C,D|T],P,Acc) ->
    partition_integer([D|T],P,[P,C,B,A|Acc]);
partition_integer(L,_,Acc) ->
    lists:reverse(L) ++ Acc.

each_with_index(L, X) -> 
    [ X(Element, Index) || {Element, Index} <- lists:zip(L, lists:seq(1, length(L))) ].


% http://schemecookbook.org/Erlang/FileCountLines
int_fold_lines(Device, Kons, Result) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Result;
   Line -> NewResult = Kons(Line, Result),
                int_fold_lines(Device, Kons, NewResult)
    end.

fold_file_lines(FileName, Kons, Knil) ->
    {ok, Device} = file:open(FileName,[read]),
    int_fold_lines(Device, Kons, Knil).

countlines(FileName) ->
  fold_file_lines(FileName, fun(L,R) -> R + 1 end, 0).

readlines(FileName) ->
    fold_file_lines(FileName, fun(L,R) -> R ++ [L] end, []).
