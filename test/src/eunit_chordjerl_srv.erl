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
         ?assert(is_record(State, srv_state) == true),
         ?assertEqual([], State#srv_state.fingers),
         ?assertEqual(simple_kv_backing_store, State#srv_state.backing_store),
         {ok}
      end
  }.

setup2() ->
     % start three nodes
     {ok, _Pid1} = chordjerl_srv:start_named(testnode1),
     {ok, _Pid2} = chordjerl_srv:start_named(testnode2),
     {ok, _Pid3} = chordjerl_srv:start_named(testnode3),
     
     % init the first node
     ok    = gen_server:call(testnode1, {create_ring}),
     {ok}.

% Todo. We're kind of breaking encapsulation with calling gen_server directly.
% but it seems preferable over writing API methods that will accept a name.
% Maybe we need api/1 calls that take the server as an argument? Maybe not.
node_network_functional_test_() ->
  {
      setup, fun setup2/0,
      fun () ->
         % grab testnode1 state
         State1 = gen_server:call(testnode1, {return_state}),
         ?NTRACE("state1 is:", [State1]),

         % join the second node to the first 
         Node = gen_server:call(testnode1, {return_node}),
         ok   = gen_server:call(testnode2, {join, Node}),
         State2 = gen_server:call(testnode2, {return_state}),
         ?NTRACE("state2 is:", [State2]),
         {srv_state, Fingers, _Predecessor, _Backing, _Sha1} = State2,
         ?assertEqual(1, length(Fingers)),
         Finger1 = lists:last(Fingers),
         {finger, _Sha2, _Node1} = Finger1,

         % here we need to stabilize and make sure the first node becomes
         % connected to the second
         % join the third node to the second
         Node2  = gen_server:call(testnode2, {return_node}),
         ok     = gen_server:call(testnode3, {join, Node2}),
         State3 = gen_server:call(testnode3, {return_state}),
         ?NTRACE("state3 is:", [State3]),
         ?assertEqual(1, length(State3#srv_state.fingers)),

         {ok, Successor1} = gen_server:call(testnode1, {find_successor, State1#srv_state.sha}),
         ?NTRACE("testnode1 find successor", [Successor1]),

         %{ok, Successor2} = gen_server:call(testnode2, {find_successor, State2#srv_state.sha}),
         Response = gen_server:call(testnode2, {find_successor, State2#srv_state.sha}),

         ?NTRACE("testnode2 find successor", [Response]),

         %%%  

         %?NTRACE("testnode2 find successor", [Successor2]),

         %{ok, Successor3} = gen_server:call(testnode3, {find_successor, State3#srv_state.sha}),
         %?NTRACE("testnode3 find successor", [Successor3]),

         {ok}
      end
  }.

successor_from_state_test_() ->
      fun () ->
         ?MOD:create_ring(),
         State = ?MOD:state(),
         ?assert(is_record(chordjerl_srv:successor(State), finger) == true),
         % ?assertEqual({ok, Finger}, chordjerl_srv:successor(State)), % when you get a successor
         {ok}
      end.
