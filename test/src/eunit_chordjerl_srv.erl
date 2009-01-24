-module(eunit_chordjerl_srv).
-include_lib("eunit/include/eunit.hrl").

-define(MOD, chordjerl_srv).

setup() ->
  % erlang:display("setup called"),
  chordjerl_srv_sup:start_in_shell_for_testing().

tracking_nodes_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         ?MOD:create_ring(),
         State = ?MOD:state(),
         ?assertEqual({state,[],undefined,simple_kv_backing_store}, State)
      end
  }.

setup2() ->
     % start three nodes
     {ok, _Pid1} = chordjerl_srv:start_named(testnode1),
     {ok, _Pid2} = chordjerl_srv:start_named(testnode2),
     {ok, _Pid3} = chordjerl_srv:start_named(testnode3),
     {ok}.

% todo, import the records so we aren't comparing against these tuples all the time
node_network_functional_test_() ->
  {
      setup, fun setup2/0,
      fun () ->
         % init the first node
         ok    = gen_server:call(testnode1, {create_ring}),
         State = gen_server:call(testnode1, {return_state}),
         ?assertEqual({state,[],undefined,simple_kv_backing_store}, State),

         % join the first node
         Node = gen_server:call(testnode1, {return_node}),
         ok   = gen_server:call(testnode2, {join, Node}),
         {state, Fingers, _predecessor, _backing} = gen_server:call(testnode2, {return_state}),
         ?assertEqual(1, length(Fingers)),
         Finger1 = lists:last(Fingers),
         {finger, _Sha, todo, none, standard} = Finger1
      end
  }.


