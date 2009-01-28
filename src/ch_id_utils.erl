%%%-------------------------------------------------------------------
%%% File        : ch_id_utils.erl
%%% Author      : Nate Murray <nate@natemurray.com>
%%% Description : Utilities for calculating ids 
%%% Created     : 2009-01-23
%%%-------------------------------------------------------------------
-module(ch_id_utils).
-compile(export_all).
-include_lib("../include/defines.hrl").

%%--------------------------------------------------------------------
%% Function: successor_id(CurrentId, Index) -> {ok,SuccessorId} 
%% Description: given CurrentId and Index returns the minimum Id required by
%% the I'th successor
%% 
%% Code ported from chord c code. Math needs a double check.
%%--------------------------------------------------------------------
successor_id(CurrentId, Index) ->
  % io:format(user, "successor for: ~p ~p~n", [CurrentId, Index]),
  B = (1 bsl ?NBIT) - 1,
  S = hex_to_int(CurrentId),
  T = (1 bsl Index),
  SuccessorId = S + T,
  SuccessorIdAnd = SuccessorId band B,
  SuccessorIdAnd.

successor_id(CurrentId) ->
  successor_id(CurrentId, 0).

%%--------------------------------------------------------------------
%% Function: hex_to_int(HexStr) -> Integer
%% Description: returns an Integer from a well-formed hex string
%% hex_to_int("7b") -> 123 
%%--------------------------------------------------------------------
hex_to_int(HexStr) ->
    {ok, Num, _} = io_lib:fread("~16u", HexStr),
    IntStr = hd(io_lib:format("~B", Num)),
    % io:format(user, "Hex: ~p Int: ~p~n", [HexStr, IntStr]),
    list_to_integer(IntStr).

%%--------------------------------------------------------------------
%% Function: bbsl
%% Description: Binary Shift Left
%% From: http://is.gd/h4Xo
%% I don't really think it works...
%%--------------------------------------------------------------------
bbsl(Bin,Shift) -> 
  <<_:Shift,Rest/bits>> = Bin, <<Rest/bits,0:Shift>>.
