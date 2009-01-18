-module(eunit_chordjerl).

-include_lib("eunit/include/eunit.hrl").

bar1_test_() ->
  [
    ?_assert(ok == chordjerl:bar1())
  ].

bar2_test_() ->
  [
    ?_assert(ok == chordjerl:bar2())
  ].
