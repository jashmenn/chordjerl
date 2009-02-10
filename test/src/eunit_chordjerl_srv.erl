-module(eunit_chordjerl_srv).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

-define(MOD, chordjerl_srv).

setup() ->
  chordjerl_srv_sup:start_in_shell_for_testing().

tracking_nodes_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         ?MOD:create_ring(),
         State = ?MOD:state(),
         ?assert(is_record(State, srv_state) =:= true),
         ?assertEqual([], State#srv_state.fingers),
         ?assertEqual(simple_kv_backing_store, State#srv_state.backing_store),
         {ok}
      end
  }.

setup2() -> 
     chordjerl_srv:start_named(testnode1),
     chordjerl_srv:start_named(testnode2),
     chordjerl_srv:start_named(testnode3),
     ?assertEqual(testnode1, gen_server:call(testnode1, {registered_name})),
     ?assertEqual(testnode2, gen_server:call(testnode2, {registered_name})),
     ?assertEqual(testnode3, gen_server:call(testnode3, {registered_name})),
     ok = gen_server:call(testnode1, {create_ring}),
     {ok}.

node_state_test_() ->
  {
      setup, fun setup2/0,
      fun () ->
         State1 = gen_server:call(testnode1, {return_state}),
         ?assert(is_record(State1, srv_state) =:= true),
         {ok}
      end
  }.

successor_from_state_test_() ->
      fun () ->
         ?MOD:create_ring(),
         State = ?MOD:state(),
         ?assert(is_record(chordjerl_srv:successor(State), finger) =:= true),
         {ok}
      end.

find_successor_test_() ->
  {
      setup, fun setup2/0,
      fun () ->
         %Response = gen_server:call(testnode2, {find_successor, State2#srv_state.sha}),
         %?NTRACE("testnode2 find successor", [Response]),
         %{ok, Successor3} = gen_server:call(testnode3, {find_successor, State3#srv_state.sha}),
         %?NTRACE("testnode3 find successor", [Successor3]),
         {ok}
      end
  }.


node_network_functional_test_() ->
  {
      setup, fun setup2/0,
      fun () ->
         Node1   = gen_server:call(testnode1, {return_finger_ref}),
         State1  = gen_server:call(testnode1, {return_state}),
         Finger1 = gen_server:call(testnode1, {immediate_successor}),

         ?assert(is_record(Node1, finger) =:= true),
         ?assertEqual(State1#srv_state.sha, Finger1#finger.sha), % Node1 successor should be itself

         % join the second node to the first 
         ok     = gen_server:call(testnode2, {join, Node1}),
         State2 = gen_server:call(testnode2, {return_state}),

         % verify fingers
         Fingers = State2#srv_state.fingers,
         Finger2 = hd(Fingers),
         Sha2    = Finger2#finger.sha,

         ?assertEqual(1, length(Fingers)),
         ?assertEqual(State1#srv_state.sha, Sha2), % first finger should now be Node1 sha

         % here we need to stabilize and make sure the first node becomes
         % connected to the second

         % join the third node to the second
         Node2   = gen_server:call(testnode2, {return_finger_ref}),
         ok      = gen_server:call(testnode3, {join, Node2}),
         State3  = gen_server:call(testnode3, {return_state}),
         Finger3 = hd(State3#srv_state.fingers),
         Sha3    = Finger3#finger.sha,

         ?assertEqual(1, length(State3#srv_state.fingers)),
         ?assertEqual(State2#srv_state.sha, Sha3), % first finger should now be Node2 sha

         {ok}
      end
  }.


