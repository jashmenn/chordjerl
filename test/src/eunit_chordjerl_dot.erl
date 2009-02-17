-module(eunit_chordjerl_dot).
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

-define(MOD, chordjerl_srv).

setup() -> 
     filelib:ensure_dir("./graphs/"),

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
     ok     = gen_server:call(testnode4, {join, Node3}), % what's happening is the node is being asked itself to find a node
     Node4  = gen_server:call(testnode4, {return_finger_ref}),

     % Shas = [{testnode1, Node1#finger.sha}, {testnode2, Node2#finger.sha}, {testnode3, Node3#finger.sha}, {testnode4, Node4#finger.sha}],
     % io:format(user, "~p~n", [lists:keysort(2, Shas)]),

     {ok}.

generate_diagram_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         write_diagram_to_file(testnode4, 0, 0),

         Max = 4,
         Iterations = 4,
         [ [ (fun() ->
                     NodeName     = list_to_atom("testnode" ++ integer_to_list(I)),
                     gen_server:call(NodeName, {stabilize}),
                     %gen_server:call(NodeName, {fix_fingers}),
                     write_diagram_to_file(testnode4, I, J)
              end)() || I <- lists:seq(1, Max) ]
         || J <- lists:seq(1, Iterations) ],

         write_diagram_to_file(testnode4, done, done),

         {ok}
      end
  }.

write_diagram_to_file(Nodename, I, J) -> 
    Response = chordjerl_dot:generate_server_graph(Nodename),
    FileName = io_lib:format("graphs/server_~p_~p.dot", [I, J]),
    io:format(user, "writing to ~s~n", [FileName]),
    {ok, FileId} = file:open(FileName, [write]),
    io:fwrite(FileId, "~s~n", [Response]),
    file:close(FileId).



setup2() ->
    {ok}.

generate_dynamic_diagram_test_() ->
% generate_dynamic_diagram() ->
  {
      setup, fun setup2/0,
      fun () ->
         Max = 12,
         [
             (fun() ->
                 PrevNodeName = list_to_atom("testnode" ++ integer_to_list(I - 1)),
                 NodeName     = list_to_atom("testnode" ++ integer_to_list(I)),
                 chordjerl_srv:start_named(NodeName),
                 case I > 1 of
                    true -> 
                        PrevNode  = gen_server:call(PrevNodeName, {return_finger_ref}),
                        ok        = gen_server:call(NodeName, {join, PrevNode});
                    _ ->
                        ok
                 end,
                 ok
             end)()
             || I <- lists:seq(1, Max)
         ],

         % stabilize each node a few times
         Iterations = 50,
         [ [ (fun() ->
                     NodeName     = list_to_atom("testnode" ++ integer_to_list(I)),
                     gen_server:call(NodeName, {stabilize})
              end)() || I <- lists:seq(1, Max) ]
         || J <- lists:seq(1, Iterations) ],

         LastNodeName = list_to_atom("testnode" ++ integer_to_list(Max)),
         write_diagram_to_file(LastNodeName, large, done),
         {ok}
      end
  }.


