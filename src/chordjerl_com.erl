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
%%
%%  Types  Finger = finger (record)
%%         Message = term()
%%         Res = term()
%%--------------------------------------------------------------------
send(Finger, Message) ->
    % rpc:call on a node doesn't work in testing because these are all the same
    % node, just different pids. just using gen_server:call till talk is ready
    gen_server:call(Finger#finger.pid, Message).
