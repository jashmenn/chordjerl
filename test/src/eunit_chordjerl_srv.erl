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

node_network_functional_test_() ->
  {
      setup, fun setup2/0,
      fun () ->
         % init the first node
         ok    = gen_server:call(testnode1, {create_ring}),
         State = gen_server:call(testnode1, {return_state}),
         ?assertEqual({state,[],undefined,simple_kv_backing_store}, State),

         % what we are trying to do here:
         % pass in to 'join' a piece of data like we are a foreign node, when
         % really we are just a pid

         % join the first node
         Node = gen_server:call(testnode1, {return_node}),
         erlang:display(Node),
         Var = gen_server:call(testnode2, {join, Node}),
         erlang:display(Var)
      end
  }.


