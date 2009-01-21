-module(eunit_chordjerl_app).

-include_lib("eunit/include/eunit.hrl").

bar1_test_() ->
  [
    ?_assert(ok == chordjerl_app:bar1())
  ].

bar2_test_() ->
  [
    ?_assert(ok == chordjerl_app:bar2())
  ].
