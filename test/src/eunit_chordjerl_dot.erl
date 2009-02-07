-module(eunit_chordjerl_dot).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

-define(MOD, chordjerl_srv).

setup() -> % todo, figure out how to tear-down
     % start three nodes
     chordjerl_srv:start_named(testnode1),
     chordjerl_srv:start_named(testnode2),
     chordjerl_srv:start_named(testnode3),

     ok     = gen_server:call(testnode1, {create_ring}),
     Node1  = gen_server:call(testnode1, {return_finger_ref}),
     ok     = gen_server:call(testnode2, {join, Node1}),
     Node2  = gen_server:call(testnode2, {return_finger_ref}),
     ok     = gen_server:call(testnode3, {join, Node2}),
     {ok}.

generate_diagram_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         io:format(user, "node1 ~p~n", [gen_server:call(testnode1, {return_state})]),
         io:format(user, "node2 ~p~n", [gen_server:call(testnode2, {return_state})]),
         io:format(user, "node3 ~p~n", [gen_server:call(testnode3, {return_state})]),

         %gen_server:call(testnode3, {stabilize}),
         %gen_server:call(testnode2, {stabilize}),
         %gen_server:call(testnode1, {stabilize}),

         %io:format(user, "stabilizing again~n", []),
         %gen_server:call(testnode3, {stabilize}),
         %gen_server:call(testnode2, {stabilize}),
         %gen_server:call(testnode1, {stabilize}),

         % connections missing:
         % * node3's predecessor should be node1.
         % * node1's successor should be node3
         % * node1's predecessor should be node2

         % the current question:
         % * how does node1 ever get a successor that is not itself?
         %   from fix_fingers? no, it seems that fix_fingers needs to know a
         %   successor to fix first

         %io:format(user, "node1 ~p~n", [gen_server:call(testnode1, {return_state})]),
         %io:format(user, "node2 ~p~n", [gen_server:call(testnode2, {return_state})]),
         %io:format(user, "node3 ~p~n", [gen_server:call(testnode3, {return_state})]),
 
         % Response = chordjerl_dot:generate_server_graph(testnode1),
         Response = chordjerl_dot:generate_server_graph(testnode3),

         % !!!! node2's finger is missing

         % starting at node 1:
         %1> 1293643317455874760360820462885806773918877262299 -> 695350099334024227710091817417021231389479344272
         %1> 695350099334024227710091817417021231389479344272 -> 1293643317455874760360820462885806773918877262299

         % starting at node 3:
         %1> 1293643317455874760360820462885806773918877262299 -> 695350099334024227710091817417021231389479344272
         %1> 689017247116779115750422726201224284066879508356 -> 1293643317455874760360820462885806773918877262299
         
         {ok, FileId} = file:open("server.dot", [write]),
         io:fwrite(FileId, "~s~n", [Response]),
         file:close(FileId),
         {ok}
      end
  }.

