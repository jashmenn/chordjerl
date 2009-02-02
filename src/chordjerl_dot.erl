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
    Nodes = collect_nodes(Pid),
    Graph = create_dot_from_nodes(Nodes),
    Graph.

collect_nodes(Pid) ->
    Node = gen_server:call(Pid, {return_state}),
    collect_nodes(Pid, [Node]).

collect_nodes(Pid, Acc) ->
    Finger1 = gen_server:call(Pid, {immediate_successor}),
    State1  = gen_server:call(Finger1#finger.pid, {return_state}),

    Matching = fun(Elem) -> (Elem#srv_state.sha == State1#srv_state.sha) end,
    case lists:any(Matching, Acc) of
        true -> 
            Acc;
        false ->
            collect_nodes(Pid, [State1|Acc])
    end.


create_dot_from_nodes(Nodes) -> 
    G = "digraph messenger {\n" ++
        "fontname = \"Bitstream Vera Sans\"\nfontsize = 9\n" ++
        "node [ fontname = \"Bitstream Vera Sans\"\n fontsize = 9\n shape = \"ellipse\"\n ]",
    G1 = G ++ lists:map(fun(Node) -> markup_for_node(Node) end, Nodes),
    G2 = G1 ++ "}\n",
    G2.

markup_for_node(Node) ->
    O  = io_lib:format("~p [label=\"~p\"]~n", [Node#srv_state.sha, gen_server:call(Node#srv_state.pid, {registered_name})]),
    O1 = O ++ lists:map(fun(Finger) -> markup_for_connection(Node, Finger) end, Node#srv_state.fingers),
    O1.

markup_for_connection(Node, Finger) ->
    io_lib:format("~p -> ~p~n", [Node#srv_state.sha, Finger#finger.sha]).
