-module(eunit_chordjerl_srv).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

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
         ?assertEqual({srv_state,[],undefined,simple_kv_backing_store}, State)
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
         ?assertEqual({srv_state,[],undefined,simple_kv_backing_store}, State),

         % join the second node to the first 
         Node = gen_server:call(testnode1, {return_node}),
         ok   = gen_server:call(testnode2, {join, Node}),
         {srv_state, Fingers, _predecessor, _backing} = gen_server:call(testnode2, {return_state}),
         ?assertEqual(1, length(Fingers)),
         Finger1 = lists:last(Fingers),
         {finger, _Sha, _Node} = Finger1,

         % here we need to stabilize and make sure the first node becomes
         % connected to the second

         % join the third node to the second
         Node2  = gen_server:call(testnode2, {return_node}),
         ok     = gen_server:call(testnode3, {join, Node2}),
         State2 = gen_server:call(testnode3, {return_state}),
         ?assertEqual(1, length(State2#srv_state.fingers)),
         io:format(user, "~p~n", [State2])
      end
  }.


