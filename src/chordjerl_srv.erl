%%%-------------------------------------------------------------------
%%% File    : chordjerl_srv.erl
%%% Author  : Nate Murray <nate@natemurray.com>
%%% Description : Chord server
%%% Created     : 2009-01-18
%%%-------------------------------------------------------------------
-module(chordjerl_srv).
-behaviour(gen_server).
-include_lib("../include/defines.hrl").

%% API
-export([
         start/0,
         start_link/0,
         start_named/1,
         create_ring/0,
         join/1,
         find_successor/1,
         closest_preceding_node/1,
         stabilize/0,
         claim_to_be_predecessor/1,
         fix_fingers/0,
         check_predecessor/0,
         state/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Alias for start_link
%%--------------------------------------------------------------------
start() ->
    start_link(). 

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% for testing multiple servers
start_named(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: create_ring() -> 
%% Description: create a new Chord ring.
%%--------------------------------------------------------------------
create_ring() ->
    gen_server:call(?SERVER, {create_ring}).

%%--------------------------------------------------------------------
%% Function: join(Node) -> 
%% Description: join a Chord ring containing Node.  
%%--------------------------------------------------------------------
join(Node) ->
    %io:format("join: the node is: ~p~n", [Node]),
    pong = net_adm:ping(Node),
    %io:format("pong: the node is: ~p~n", [Node]),
    gen_server:call(?SERVER, {join, Node}).

%%--------------------------------------------------------------------
%% Function: find_successor(Id) -> 
%% Description: find the successor of Id
%%--------------------------------------------------------------------
find_successor(Id) ->
    gen_server:call(?SERVER, {find_successor, Id}).

%%--------------------------------------------------------------------
%% Function: closest_preceding_node(Id) -> 
%% Description: searc the local table for the highest predecessor if id
%%--------------------------------------------------------------------
closest_preceding_node(Id) ->
    gen_server:call(?SERVER, {closest_preceding_node, Id}).

%%--------------------------------------------------------------------
%% Function: stabilize() -> 
%% Description: called periodically. veriﬁes immediate successor, and tells the
%%              successor about this node. 
%%--------------------------------------------------------------------
stabilize() ->
    gen_server:call(?SERVER, {stabilize}).

%%--------------------------------------------------------------------
%% Function: claim_to_be_predecessor(Node) -> 
%% Description: Node thinks it might be our predecessor
%%--------------------------------------------------------------------
claim_to_be_predecessor(Node) ->
    gen_server:call(?SERVER, {claim_to_be_predecessor, Node}).

%%--------------------------------------------------------------------
%% Function: fix_fingers() -> 
%% Description: called periodically. refreshes ﬁnger table entries.  
%%              next stores the index of the next finger to fix.
%%--------------------------------------------------------------------
fix_fingers() ->
    gen_server:call(?SERVER, {fix_fingers}).

%%--------------------------------------------------------------------
%% Function: check_predecessor() -> 
%% Description: called periodically. checks whether predecessor has 
%%              failed.
%%--------------------------------------------------------------------
check_predecessor() ->
    gen_server:call(?SERVER, {check_predecessor}).

state() ->
    gen_server:call(?SERVER, {return_state}).

%sha() ->
    %"1234".

get_node() ->
    gen_server:call(?SERVER, {return_node}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    IdString = atom_to_list(node()) ++ pid_to_list(self()),  % not sure about this
    Sha = sha1:hexstring(IdString), 
    ShaInt = hex_to_int(HexStr),              % for now, just store the finger as an int
    {ok, #srv_state{sha=ShaInt}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create_ring}, _From, State) ->
    {Reply, NewState} = handle_create_ring(State),
    {reply, Reply, NewState};

handle_call({join, Node}, _From, State) ->
    {Reply, NewState} = handle_join(Node, State),
    {reply, Reply, NewState};

handle_call({find_successor, Id}, _From, State) ->
    {Reply, NewState} = handle_find_successor(Id, State),
    {reply, Reply, NewState};

handle_call({closest_preceding_node, Id}, _From, State) ->
    {Reply, NewState} = handle_closest_preceding_node(Id, State),
    {reply, Reply, NewState};

handle_call({stabilize}, _From, State) ->
    {Reply, NewState} = handle_stabilize(State),
    {reply, Reply, NewState};

handle_call({claim_to_be_predecessor, Node}, _From, State) ->
    {Reply, NewState} = handle_claim_to_be_predecessor(Node, State),
    {reply, Reply, NewState};

handle_call({fix_fingers}, _From, State) ->
    {Reply, NewState} = handle_fix_fingers(State),
    {reply, Reply, NewState};

handle_call({check_predecessor}, _From, State) ->
    {Reply, NewState} = handle_check_predecessor(State),
    {reply, Reply, NewState};

handle_call({return_state}, _From, State) ->
    Reply = State,
    {reply, Reply, State};

handle_call({return_node}, _From, State) ->
    Reply = erlang:node(),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% handle_* Internal functions
%%--------------------------------------------------------------------
handle_create_ring(State) ->
    NewState = State#srv_state{predecessor=undefined, fingers=[]},
    {ok, NewState}.

handle_join(Node, State) ->
    %io:format("in join: the node is: ~p~n", [Node]),
    % pong = net_adm:ping(Node),
    NewSuccessor = rpc:call(Node, ?SERVER, find_successor, [State#srv_state.sha]),
    %io:format("rpcd: the node is: ~p~n", [Node]),
    %io:format("NewSuccessor is: ~p~n", [NewSuccessor]),
    {ok, NewFinger} = make_finger(NewSuccessor), 
    NewFingers   = [NewFinger|State#srv_state.fingers],
    NewState     = State#srv_state{predecessor=undefined,fingers=NewFingers},
    {ok, NewState}.

handle_find_successor(Id, State) ->
    % if Id between State#srv_state.sha...successor_id(State))
    %   return successor
    % else
    %   NewNode = closest_preceding_node(Id)
    %   rpc:call(NewNode, ?SERVER, find_successor, [Id])
    {todo, State}.

handle_closest_preceding_node(Id, State) ->
    {todo}.

handle_stabilize(State) ->
    {todo}.

handle_claim_to_be_predecessor(Node, State) -> 
    {todo}.

handle_fix_fingers(State) ->
    {todo}.

handle_check_predecessor(State) ->
    {todo}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
  
%%%%%%%%% TODO - this needs to have the node sha passed in, probably... set the sha in *one* place. in init
make_finger(Node) ->
  %io:format("~p finger: the node is: ~p~n", [node(), Node]),
  Sha = sha1:hexstring(atom_to_list(Node)), % no, the sha should already exist TODO
  ShaInt = hex_to_int(HexStr),              % for now, just store the finger as an int
  {ok, #finger{node=Node, sha=Sha}}.

%%--------------------------------------------------------------------
%% Function: successor_id(State) -> Integer
%% Description: returns the minimum integer for the successor's id
%%--------------------------------------------------------------------
successor_id(State) ->
  ch_id_utils:successor_id(State#srv_state.sha, 1).
  


%
% Networking methods, to be exchanged with erltalk in time
%
connect_to_node(NodeLocation) ->
    case net_adm:ping(NodeLocation) of
        pong ->
            global:sync(),
            ok;
        _ ->
            receive
                stop -> void
            after ?RECONNECT_TIMEOUT ->
                connect_to_node(NodeLocation)
            end
end.
