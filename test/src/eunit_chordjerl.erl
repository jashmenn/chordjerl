-module(eunit_chordjerl).
-export([bar1/0, bar2/0]).

-include_lib("eunit/include/eunit.hrl").

bar1_test_() ->
  [
    ?_assert(ok == bar1())
  ].

bar2_test_() ->
  [
    ?_assert(ok == bar2())
  ].
