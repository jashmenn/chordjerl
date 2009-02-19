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

create_dot_from_nodes(UnsortedNodes) -> 
    Nodes = sort_nodes_by_sha(UnsortedNodes),
    G = "digraph messenger {\n" ++
        "fontname = \"Bitstream Vera Sans\"\nfontsize = 9\n" ++
        "node [ fontname = \"Bitstream Vera Sans\"\n fontsize = 9\n shape = \"ellipse\"\n ]\n" ++
        "edge [ fontname = \"Bitstream Vera Sans\"\n fontsize = 9\n ]\n",
    %G4 = G ++ markup_for_guiding_connections(Nodes),
    G4 = G ++ "",
    G1  = G4  ++ lists:map(fun(Node) -> markup_for_node(Node) end, Nodes),
    G11 = G1  ++ lists:map(fun(Node) -> markup_for_node_successor_edges(Node) end, Nodes),
    %G12 = G11 ++ lists:map(fun(Node) -> markup_for_node_finger_edges(Node) end, Nodes),
    G3 = G11 ++ "subgraph cluster_fingertables {rake=min; " ++ lists:map(fun(Node) -> markup_for_finger_table(Node) end, Nodes),
    G5 = G3 ++ ch_utils:each_with_index(Nodes, 
        fun(Node, Index) ->
            case Index > 1 andalso ((Index rem 4) =/= 0) of
                true -> 
                  PreviousNode = lists:nth(Index - 1, Nodes),
                  io_lib:format("finger_table_~p -> finger_table_~p~n", [PreviousNode#srv_state.sha, Node#srv_state.sha]);
                false ->
                  " "
            end
        end),

    G6 = G5 ++ "}\n",
    G7 = G6 ++ "}\n",
    {G8, TmpCircoFileName} = preprocess_with_circo(Nodes, G7),
    ProcessedCircoFileName = add_finger_tables_to_circo_file(TmpCircoFileName, Nodes),
    G9 = contents_of_file(ProcessedCircoFileName),
    G9.

% similar to ruby's #each_with_index:
% lists:zip(L, lists:seq(1, length(L))).
markup_for_node(Node) ->
    O  = io_lib:format("~p [label=\"~p\\n~p\\n~p\"]~n", 
            [Node#srv_state.sha, 
            gen_server:call(Node#srv_state.pid, {registered_name}), 
                            Node#srv_state.pid,
                            Node#srv_state.sha]), 
    O.

markup_for_node_successor_edges(Node) ->
    O1 = markup_for_connection(Node, hd(Node#srv_state.fingers), 1),
    O1.

markup_for_node_finger_edges(Node) ->
    FingerTableLen = length(Node#srv_state.fingers),
    O1 = ch_utils:each_with_index(Node#srv_state.fingers, 
        fun(Finger, Index) ->
            case Index > 1 of
                true -> 
                  markup_for_connection(Node, Finger, Index);
                false ->
                  " "
            end
        end),
    O1.

    %O3 = O1 ++ markup_for_predecessor(Node, Node#srv_state.predecessor),
    %O3.

markup_for_connection(Node, Finger, Index) ->
    {Color, Weight, Prefix} = case Index > 1 of
        true ->
          {"gray80", 0, "// AFTER"};
        false ->
          {"gray0", 2, ""}
    end,
    case Index > 1 andalso 
         lists:nth(Index, Node#srv_state.fingers) =:= lists:nth(Index - 1, Node#srv_state.fingers) of
        true -> []; % skip it
        false -> io_lib:format("~p -> ~p [label=~p,constraint=~p,color=~p,weight=~p]~n", [Node#srv_state.sha, Finger#finger.sha, Index, Index < 2, Color, Weight])
    end.

markup_for_predecessor(_Node, undefined) ->
    [];
markup_for_predecessor(Node, Finger) ->
  % io_lib:format("~p -> ~p [style=dashed,arrowhead=open,constraint=false]~n", [Node#srv_state.sha, Finger#finger.sha]).
    io_lib:format("~p -> ~p [style=invis,arrowhead=open,constraint=false]~n", [Node#srv_state.sha, Finger#finger.sha]).

markup_for_finger_table(Node) ->
    Fingers = [io_lib:format("~p: ~p  (+~p gte ~p)\\l", [Index, Finger#finger.sha, round(math:pow(2, Index - 1)), ch_id_utils:successor_id(Node#srv_state.sha, Index)]) || {Finger, Index} <- lists:zip(Node#srv_state.fingers, lists:seq(1, length(Node#srv_state.fingers)))],
    Name = gen_server:call(Node#srv_state.pid, {registered_name}),
    O  = io_lib:format("finger_table_~p [label=\"{~p fingers (~p)|~s}\", shape=record]~n", [Node#srv_state.sha, Name, Node#srv_state.sha, Fingers]),
    O.

markup_for_guiding_connections(Nodes) ->
    G   = "{\nrake = same;\nedge [style=invis];\ntopnode [shape=record, style=invis];\n",
    Pids = lists:sort([Node#srv_state.sha || Node <- Nodes]),
    G10 = G ++ [io_lib:format("topnode:~p -> ~p~n", [Pid, Pid]) || Pid <- Pids],
    G20 = G10 ++ "}\n",
    G30 = G20.

sort_nodes_by_sha(Nodes) ->
    lists:sort(fun(Elem1, Elem2) ->
        Elem1#srv_state.sha < Elem2#srv_state.sha
    end,
    Nodes).

write_diagram_to_file(Nodename, I, J) -> 
    Response = chordjerl_dot:generate_server_graph(Nodename),
    FileName = io_lib:format("graphs/server_~p_~p.dot", [I, J]),
    save_output_to_file(Response, FileName).

save_output_to_file(Output, FileName) ->
    io:format(user, "writing to ~s~n", [FileName]),
    {ok, FileId} = file:open(FileName, [write]),
    io:fwrite(FileId, "~s", [Output]),
    file:close(FileId),
    FileName.

preprocess_with_circo(Nodes, Output) ->
    TmpFileName      =  "/tmp/server_pre_circo.dot", % !! not thread safe, choose the Pid instead
    TmpCircoFileName =  "/tmp/server_circo.dot", % !! not thread safe, choose the Pid instead
    save_output_to_file(Output, TmpFileName),
    CircoCmd = io_lib:format("circo ~p > ~p", [TmpFileName, TmpCircoFileName]),
    os:cmd(CircoCmd),
    {Output, TmpCircoFileName}.

% sooo hackish. please no one read this method. nothing to see here, move along.
add_finger_tables_to_circo_file(FileName, Nodes) ->
    TmpFile = FileName ++ ".tmp",
    os:cmd(io_lib:format("sed '$d' ~p > ~p", [FileName, TmpFile])),
    Out   = lists:map(fun(Node) -> markup_for_node_finger_edges(Node) end, Nodes),
    Out10 = Out ++ "}\n",
    {ok, FileId} = file:open(TmpFile, [append]),
    io:fwrite(FileId, "~s", [Out10]),
    file:close(FileId),
    TmpFile.

contents_of_file(FileName) ->
    ch_utils:readlines(FileName).

render_file(Filename) ->
    Format = "png",
    RenderedFile = io_lib:format("~s.~s", [Filename, Format]),
    Cmd = io_lib:format("neato -s -n2 -T~s -o ~s ~s", [Format, RenderedFile, Filename]),
    io:format(user, "~s", [Cmd]),
    os:cmd(Cmd),
    Filename.


% next -- do the sed cmd, and append the nodes finger table connectiosn to the end of the file and then save to the right place

