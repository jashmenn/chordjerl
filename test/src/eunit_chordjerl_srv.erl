-module(eunit_chordjerl_srv).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

-define(MOD, chordjerl_srv).

-define(assertSuccessorShaEqual(State, Sha),
    ((fun () ->
        Finger = hd(State#srv_state.fingers),
        ?assertEqual(Sha, Finger#finger.sha)
      end)())).

-define(join_node_to_node(Pid1, Pid2),
    ((fun () ->
        F1 = gen_server:call(Pid1, {return_finger_ref}),
        ok = gen_server:call(Pid2, {join, F1})
      end)())).

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
         State1  = gen_server:call(testnode1, {return_state}),
         State2  = gen_server:call(testnode2, {return_state}),
         State3  = gen_server:call(testnode3, {return_state}),

         {ok, _Successor1} = gen_server:call(testnode1, {find_successor, State1#srv_state.sha}),
         {ok, _Successor2} = gen_server:call(testnode2, {find_successor, State2#srv_state.sha}),
         {ok, _Successor3} = gen_server:call(testnode3, {find_successor, State3#srv_state.sha}),
         {ok}
      end
  }.


node_network_functional_test_() ->
  {
      setup, fun setup2/0,
      fun () ->
         State1  = gen_server:call(testnode1, {return_state}),
         Finger1 = gen_server:call(testnode1, {immediate_successor}),
         ?assertEqual(State1#srv_state.sha, Finger1#finger.sha),  % Node1 successor should be itself

         % join the second node to the first 
         ?join_node_to_node(testnode1, testnode2), 
         State2 = gen_server:call(testnode2, {return_state}),
         ?assertSuccessorShaEqual(State2, State1#srv_state.sha), % first finger should now be Node1 sha

         % join the third node to the second 
         ?join_node_to_node(testnode2, testnode3), 
         State3  = gen_server:call(testnode3, {return_state}),
         ?assertSuccessorShaEqual(State3, State1#srv_state.sha), % first finger should now be Node1 sha

         {ok}
      end
  }.

