%%%-------------------------------------------------------------------
%%% File    : chordjerl_srv.erl
%%% Author  : Nate Murray <nate@natemurray.com>
%%% Description : Chord server
%%% Created     : 2009-01-18
%%%-------------------------------------------------------------------
-module(chordjerl_srv).
-behaviour(gen_server).
-include_lib("../include/defines.hrl").
-compile(export_all).

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
         return_predecessor/0,
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
%% Function: join(OtherNode) -> 
%% Description: join a Chord ring containing Node.  
%%--------------------------------------------------------------------
join(OtherNode) ->
    gen_server:call(?SERVER, {join, OtherNode}).

%%--------------------------------------------------------------------
%% Function: find_successor(Id) -> 
%% Description: find best/cloest known successor of Id
%%--------------------------------------------------------------------
find_successor(Id) ->
    gen_server:call(?SERVER, {find_successor, Id}).

%%--------------------------------------------------------------------
%% Function: immediate_successor() -> 
%% Description: return the successor of this node
%%--------------------------------------------------------------------
immediate_successor() ->
    gen_server:call(?SERVER, {immediate_successor}).

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
%% Description: Node thinks it might be our predecessor. (Listed as 'notify' in
%% Chord paper)
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

%%--------------------------------------------------------------------
%% Function: return_predecessor() -> {finger, Finger} | false
%% Description: returns the predecessor of this node
%%--------------------------------------------------------------------
return_predecessor() ->
    gen_server:call(?SERVER, {return_predecessor}).

state() ->
    gen_server:call(?SERVER, {return_state}).

%%--------------------------------------------------------------------
%% Function: get_finger_ref() -> Finger
%% Description: returns a reference to this node in finger format
%%--------------------------------------------------------------------
get_finger_ref() ->
    gen_server:call(?SERVER, {return_finger_ref}).

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
    ShaInt = make_sha([]),
    {ok, #srv_state{sha=ShaInt,pid=self(),predecessor=undefined}}.

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

handle_call({join, OtherNode}, _From, State) ->
    {Reply, NewState} = handle_join(OtherNode, State),
    {reply, Reply, NewState};

handle_call({find_successor, Id}, _From, State) ->
    {Reply, NewState} = handle_find_successor(Id, State),
    {reply, Reply, NewState};

handle_call({immediate_successor}, _From, State) ->
    {Reply, NewState} = handle_immediate_successor(State),
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

handle_call({return_predecessor}, _From, State) ->
    Reply = handle_return_predecessor(State),
    {reply, Reply, State};

handle_call({return_finger_ref}, _From, State) ->
    {Reply, NewState} = handle_return_finger_ref(State),
    {reply, Reply, NewState};

handle_call({registered_name}, _From, State) ->
    Reply = registered_name(),
    {reply, Reply, State};

handle_call({joined_by, Finger}, _From, State) ->
    {ok, NewState} = handle_joined_by(Finger, State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = invalid,
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

handle_join(Finger, State) ->
    Response = chordjerl_com:send(Finger, {find_successor, State#srv_state.sha}),
    case Response of
        {ok, NewFinger} -> 
            NewFingers   = [NewFinger|State#srv_state.fingers],
            NewState     = State#srv_state{fingers=NewFingers},
            _Response = chordjerl_com:send(Finger, {joined_by, make_finger_from_self(State)}), % tell the node we joined it
            {ok, NewState};
        _Err ->
            ?NTRACE("bad response", Response),
            {uhh, State} % todo
    end.

% if you were joined by another node but you don't have any successors (for
% instance, you are the first node in your ring) then add the node that joined
% you to your successor list
handle_joined_by(Finger, State) when length(State#srv_state.fingers) =:= 0 ->
    NewFingers   = [Finger|State#srv_state.fingers],
    NewState     = State#srv_state{fingers=NewFingers},
    {ok, NewState};
handle_joined_by(_Finger, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_find_successor(Id, State) -> {{ok, SuccessorFinger}, NewState}
%% Description: find the successor of Id
%% returns in finger format
%%--------------------------------------------------------------------
handle_find_successor(Id, State) -> % could use a refactoring...
    SuccessorFinger = successor(State),
    SuccessorId = SuccessorFinger#finger.sha,
    case State#srv_state.sha =:= SuccessorId of
        true ->
            {{ok, SuccessorFinger}, State}; % if successor is self, return self
        false ->
            case ch_id_utils:id_in_segment(State#srv_state.sha, SuccessorId, Id) of
                true  -> 
                   {{ok, SuccessorFinger}, State};
                false -> % find recursively
                   {{ok, Finger}, _NewState} = handle_closest_preceding_node(Id, State),
                   case Finger#finger.pid =:= self() of                      
                     true ->
                        {{ok, Finger}, State};
                     false ->
                        chordjerl_com:send(Finger, {find_successor, Id})
                   end
            end
    end.

handle_immediate_successor(State) ->
    {successor(State), State}.

handle_closest_preceding_node(Id, State) ->
    FingersR = lists:reverse(State#srv_state.fingers), % fingers are stored in ascending order
    handle_closest_preceding_node(Id, State, FingersR).

handle_closest_preceding_node(Id, State, [Finger|T]) ->
    case ch_id_utils:id_in_segment(State#srv_state.sha, Finger#finger.sha, Id) of
        true  -> 
           {{ok, Finger}, State};
        false -> 
           handle_closest_preceding_node(Id, State, T) 
    end;
handle_closest_preceding_node(_Id, State, []) ->
    {{ok, make_finger_from_self(State)}, State}.


%%--------------------------------------------------------------------
%% Function: handle_stabilize(State) -> 
%% Description: called periodically. verifies immediate successor, and tells the
%%              successor about this node. 
%%--------------------------------------------------------------------
handle_stabilize(State) ->
    {Successor, _State} = handle_immediate_successor(State), 
    case Successor#finger.sha =:= State#srv_state.sha of
        true ->
            {ok, State}; % our sucessor is ourself, don't do anything
        false ->
            handle_stabilize(State, Successor)
    end.

%%--------------------------------------------------------------------
%% Function: handle_stabilize(State, Successor) -> 
%% Arguments: Successor must not be self as a finger 
%%--------------------------------------------------------------------
handle_stabilize(State, Successor) ->
    SuccPred = chordjerl_com:send(Successor, {return_predecessor}),
    {RealSuccessor, NewState} = case is_record(SuccPred, finger) of
        true -> 
            case ch_id_utils:id_in_segment(State#srv_state.sha, 
                                           Successor#finger.sha, 
                                           SuccPred#finger.sha) of
                true  -> 
                    % SuccPred is our real Successor this is a State changing
                    % operation, not just a notification of SuccPred
                    {ok, NewState1} = handle_set_immediate_successor(SuccPred, State),
                    {SuccPred, NewState1};
                false -> 
                    {Successor, State}
            end;
        false -> % if Successor has no predecessor, then just notify Seccessor
            {Successor, State}
    end,

    {SelfAsFinger, _State} = handle_return_finger_ref(State),
    Response = chordjerl_com:send(RealSuccessor, {claim_to_be_predecessor, SelfAsFinger}),
    {Response, NewState}.

handle_return_predecessor(State) ->
    case is_record(State#srv_state.predecessor, finger) of
        true -> 
            {finger, State#srv_state.predecessor};
        false -> 
            undefined
    end.

handle_claim_to_be_predecessor(Node, State) when is_record(Node, finger) -> 
    Predecessor = handle_return_predecessor(State),
    {Response, NewState} = if
        undefined =:= Predecessor -> 
            handle_set_new_predecessor(Node, State);
        is_record(Predecessor, finger) ->
            % is Node between our current Predecessor and us?
            case ch_id_utils:id_in_segment( Predecessor#finger.sha,
                                            State#srv_state.sha, 
                                            Node#finger.sha) of
               true ->
                    handle_set_new_predecessor(Node, State);
               false ->
                    {nochange, State}
            end;
        true ->
          {nochange, State}
    end,
    {Response, NewState}.

handle_set_new_predecessor(Node, State) ->
    % io:format(user, "setting new predecessor for ~p ~p to ~p ~p~n", 
    %     [State#srv_state.sha, State#srv_state.pid, Node#finger.sha, Node#finger.pid]),
    NewState = State#srv_state{predecessor=Node},
    {ok, NewState}.

handle_fix_fingers(_State) ->
    {todo}.

handle_check_predecessor(_State) ->
    {todo}.

handle_return_finger_ref(State) ->
    {make_finger_from_self(State), State}.

%%-----------------------------------------------------------------------------
%% Function: 
%% Description: 
%%-----------------------------------------------------------------------------
handle_set_immediate_successor(NewSuccessor, State) ->
    % io:format(user, "setting new successor for ~p ~p to ~p ~p~n", 
    %      [State#srv_state.sha, State#srv_state.pid, NewSuccessor#finger.sha, NewSuccessor#finger.pid]),
    NewState = State#srv_state{fingers=[NewSuccessor|State#srv_state.fingers]},
    {ok, NewState}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
  
%%--------------------------------------------------------------------
%% Function: successor(State) -> {ok, #finger} | {none} 
%% Description: returns the immediate successor of this node. 
%%--------------------------------------------------------------------
successor(State) when length(State#srv_state.fingers) >= 1 ->
    hd(State#srv_state.fingers);
successor(State) -> % if no successors then return self as finger
    make_finger_from_self(State).

make_finger_from_self(State) ->
    #finger{sha=State#srv_state.sha, node=node(), pid=self()}.

%%-----------------------------------------------------------------------------
%% Function: registered_name(State) -> {ok, Name} | false
%% Description: returns the registered name of this process or false if the
%% process is not registered.  %% This should only be used in debugging or in
%% tests.
%% It is used, if available, for consistent hashing of the nodes which, in turn,
%% makes them testable.
%%-----------------------------------------------------------------------------
registered_name() ->
    case lists:keysearch(registered_name, 1, process_info(self())) of
        {value, {registered_name, Name}} ->
            Name;
        _ ->
            false
    end.

make_sha([]) ->
    % use the registered name if we have one. This helps ensure consistent
    % hashing while testing. However, we may want to switch this up for
    % production.
    Scope = case registered_name() of
        false ->
            pid_to_list(self());
        Name -> 
            atom_to_list(Name)
    end,

    IdString = atom_to_list(node()) ++ Scope,  % not sure about this
    Sha = sha1:hexstring(IdString), 
    ch_id_utils:hex_to_int(Sha).               % for now, just store the finger as an int
