%%%-------------------------------------------------------------------
%%% File    : chordjerl_com.erl
%%% Author  : Nate Murray <nate@natemurray.com>
%%% Description : Abstraction for Chordjerl communication between nodes
%%%   It will eventually use 'talk' but just uses distrubted erlang for now
%%% Created     : 2009-01-30
%%%-------------------------------------------------------------------
-module(chordjerl_com).
-include_lib("../include/defines.hrl").
-compile(export_all).


%%--------------------------------------------------------------------
%% Function: send(Finger, Message) -> Res
%% Description: Sends Finger Message
%% todo, should be called "call"
%%
%%  Types  Finger = finger (record)
%%         Message = term()
%%         Res = term()
%%--------------------------------------------------------------------
send(Finger, Message) -> 
    % just using gen_server:call till 'converse' is ready
    gen_server:call(Finger#finger.pid, Message).

cast(Finger, Message) ->
    gen_server:cast(Finger#finger.pid, Message).
