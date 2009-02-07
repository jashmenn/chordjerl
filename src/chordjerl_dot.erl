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
    % io:format(user, "~n~nnodes: ~p", [Nodes]),
    Graph = create_dot_from_nodes(Nodes),
    Graph.

collect_nodes_better(Pid) ->
   D  = dict:new(),
   collect_nodes_better(Pid, D).

collect_nodes_better(Pid, D) ->
    io:format(user, "the pid is: ~p", [Pid]),
   Node = gen_server:call(Pid, {return_state}),
   D1 = collect_all_fingers(Node, D),
    io:format(user, "~n~nD1 of nodes is: ~p", [D1]),
   % then convert D1 to list of fingers
   [].

% if the dict has all of your fingers, return the dict
% otherwise, recurse to add the fingers of that finger 
collect_all_fingers(Node, D) -> 
   % then convert D1 to list of fingers
   D0 = dict:store(Node#srv_state.sha, Node, D),
   Fingers = Node#srv_state.fingers,
   % loop over all fingers
   % if the dict has the key of the sha, then next
   % if the dict does not have the key of the sha, then collect_all_fingers on that Node
   % and at some point add the current node to the list. how about always add self to dict, assume it wont be called if it wasn't needed
   D1 = lists:foldl(  
       fun(Finger, D2) -> 
           % io:format(user, "trying to get a node for ~p~n", [Finger#finger.pid]),
           Node1 = gen_server:call(Finger#finger.pid, {return_state}),
           % io:format(user, "node is ~p~n", [Node1]),
           case dict:is_key(Node#srv_state.sha, D2) of
                true ->
                    D2;
                false ->
                    collect_all_fingers(Node1, D2)
           end
       end,
       D0, Fingers),

   D1.


collect_nodes(Pid) ->
    Node = gen_server:call(Pid, {return_state}),
    collect_nodes(Pid, [Node]).

collect_nodes(Pid, Acc) ->
    Finger1 = gen_server:call(Pid, {immediate_successor}),
    State1  = gen_server:call(Finger1#finger.pid, {return_state}),

    Matching = fun(Elem) -> (Elem#srv_state.sha == State1#srv_state.sha) end,
    case lists:any(Matching, Acc) of
        true -> % terminates when his a duplicate node. could probably be improved
            % io:format(user, "~nfound sha ~p in Acc", [State1#srv_state.sha]),
            Acc;
        false ->
            collect_nodes(Pid, [State1|Acc])
    end.



create_dot_from_nodes(Nodes) -> 
    G = "digraph messenger {\n" ++
        "fontname = \"Bitstream Vera Sans\"\nfontsize = 9\n" ++
        "node [ fontname = \"Bitstream Vera Sans\"\n fontsize = 9\n shape = \"ellipse\"\n ]\n",
    G1 = G ++ lists:map(fun(Node) -> markup_for_node(Node) end, Nodes),
    G2 = G1 ++ "}\n",
    G2.

markup_for_node(Node) ->
    O  = io_lib:format("~p [label=\"~p\\n~p\"]~n", [Node#srv_state.sha, gen_server:call(Node#srv_state.pid, {registered_name}), Node#srv_state.pid]),
    O1 = O  ++ lists:map(fun(Finger) -> markup_for_connection(Node, Finger) end, Node#srv_state.fingers),
    O2 = O1 ++ markup_for_predecessor(Node, Node#srv_state.predecessor) ,
    O2.

markup_for_connection(Node, Finger) ->
    io:format(user, "~p -> ~p~n", [Node#srv_state.sha, Finger#finger.sha]),
    io_lib:format("~p -> ~p~n", [Node#srv_state.sha, Finger#finger.sha]).

markup_for_predecessor(_Node, undefined) ->
    [];
markup_for_predecessor(Node, Finger) ->
    io_lib:format("~p -> ~p [style=dashed]~n", [Node#srv_state.sha, Finger#finger.sha]).
