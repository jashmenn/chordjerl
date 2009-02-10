-module(eunit_chordjerl_dot).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

-define(MOD, chordjerl_srv).

setup() -> % todo, figure out how to tear-down
     % start three nodes
     chordjerl_srv:start_named(testnode1),
     chordjerl_srv:start_named(testnode2),
     chordjerl_srv:start_named(testnode3),
     chordjerl_srv:start_named(testnode4),

     ok     = gen_server:call(testnode1, {create_ring}),
     Node1  = gen_server:call(testnode1, {return_finger_ref}),
     ok     = gen_server:call(testnode2, {join, Node1}),
     Node2  = gen_server:call(testnode2, {return_finger_ref}),
     ok     = gen_server:call(testnode3, {join, Node2}),
     Node3  = gen_server:call(testnode3, {return_finger_ref}),
     ok     = gen_server:call(testnode4, {join, Node3}),
     {ok}.

generate_diagram_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         gen_server:call(testnode4, {stabilize}),
         gen_server:call(testnode3, {stabilize}),
         gen_server:call(testnode2, {stabilize}),
         gen_server:call(testnode1, {stabilize}),

         % stabilize here should set testnode3 as testnode1's successor because
         % stabilize should be having testnode1 asking testnode2 'who is your predecessor?'
         % testnode2 should respond "testnode3". testnode1 should see that testnode3 is within the 
         % right segment and therefore set testnode3 as its successor

         % io:format(user, "node1 ~p~n", [gen_server:call(testnode1, {return_state})]),
         % io:format(user, "node2 ~p~n", [gen_server:call(testnode2, {return_state})]),
         % io:format(user, "node3 ~p~n", [gen_server:call(testnode3, {return_state})]),

         % gen_server:call(testnode1, {fix_fingers}),
         % gen_server:call(testnode2, {fix_fingers}),
         % gen_server:call(testnode3, {fix_fingers}),

         % connections missing:
         % * node3's predecessor should be node1
         % * node1's successor   should be node3

         Response = chordjerl_dot:generate_server_graph(testnode4),
         {ok, FileId} = file:open("server.dot", [write]),
         io:fwrite(FileId, "~s~n", [Response]),
         file:close(FileId),
         {ok}
      end
  }.

