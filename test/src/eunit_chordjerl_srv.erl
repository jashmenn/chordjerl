-module(eunit_chordjerl_srv).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  erlang:display("setup called"),
  chordjerl_srv_sup:start_in_shell_for_testing().

tracking_nodes_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         Nodes = chordjerl_srv:ch_nodes(),
         ?assert(Nodes =:= []),
         erl_pp:attribute(Nodes, fun erlang:display/1)
      end
  }.
