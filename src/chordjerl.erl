%%% 
-module(chordjerl).
-export([bar1/0, bar2/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.


bar1() ->
  ok.

bar2() ->
  ok.

-ifdef(EUNIT).
bar1_test_() ->
  [
    ?_assert(ok == bar1())
  ].
-endif.

-ifdef(EUNIT).
bar2_test_() ->
  [
    ?_assert(ok == bar2())
  ].
-endif.
