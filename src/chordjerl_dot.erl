%%%-------------------------------------------------------------------
%%% File    : chordjerl_dot.erl
%%% Author  : Nate Murray <nate@natemurray.com>
%%% Description : Generate dot graphs of server rings
%%% Created     : 2009-02-01
%%%
%%% circo server.dot | dot -Tpng -o server.png
%%%-------------------------------------------------------------------
-module(chordjerl_dot).
-include_lib("../include/defines.hrl").
-compile(export_all).

generate_server_graph(Pid) ->
    Nodes = collect_nodes_better(Pid),
    Graph = create_dot_from_nodes(Nodes),
    Graph.

collect_nodes_better(Pid) ->
   D  = dict:new(),
   collect_nodes_better(Pid, D).

collect_nodes_better(Pid, D) ->
   Node = gen_server:call(Pid, {return_state}),
   D1 = collect_all_fingers(Node, D),
   {Keys, Values} = lists:unzip(dict:to_list(D1)),
   Values.

% loop over all fingers
% if the dict has the key of the sha, then next
% if the dict does not have the key of the sha, then recurse on that Node
collect_all_fingers(Node, D) -> 
   % then convert D1 to list of fingers
   D0 = dict:store(Node#srv_state.sha, Node, D),
   Fingers = Node#srv_state.fingers,
   D1 = lists:foldl(  
       fun(Finger, D2) -> 
           Node1 = gen_server:call(Finger#finger.pid, {return_state}),
           case dict:is_key(Node1#srv_state.sha, D2) of
                true ->
                    D2;
                false ->
                    collect_all_fingers(Node1, D2)
           end
       end,
       D0, Fingers),
   D1.

create_dot_from_nodes(Nodes) -> 
    G = "digraph messenger {\n" ++
        "fontname = \"Bitstream Vera Sans\"\nfontsize = 9\n" ++
        "node [ fontname = \"Bitstream Vera Sans\"\n fontsize = 9\n shape = \"ellipse\"\n ]\n",
    G1 = G ++ lists:map(fun(Node) -> markup_for_node(Node) end, Nodes),
    G2 = G1 ++ "}\n",
    G2.

markup_for_node(Node) ->
    O  = io_lib:format("~p [label=\"~p\\n~p\\n~p\"]~n", 
            [Node#srv_state.sha, 
            gen_server:call(Node#srv_state.pid, {registered_name}), 
                            Node#srv_state.pid,
                            Node#srv_state.sha rem ?NBIT]),
    O1 = O  ++ lists:map(fun(Finger) -> markup_for_connection(Node, Finger) end, Node#srv_state.fingers),
    O2 = O1 ++ markup_for_predecessor(Node, Node#srv_state.predecessor) ,
    O2.

markup_for_connection(Node, Finger) ->
    io_lib:format("~p -> ~p~n", [Node#srv_state.sha, Finger#finger.sha]).

markup_for_predecessor(_Node, undefined) ->
    [];
markup_for_predecessor(Node, Finger) ->
    io_lib:format("~p -> ~p [style=dashed]~n", [Node#srv_state.sha, Finger#finger.sha]).
