%%%-------------------------------------------------------------------
%%% File        : ch_id_utils.erl
%%% Author      : Nate Murray <nate@natemurray.com>
%%% Description : Utilities for calculating ids 
%%% Created     : 2009-01-23
%%%-------------------------------------------------------------------
-module(ch_id_utils).
-compile(export_all).

%%--------------------------------------------------------------------
%% Function: successor_id(CurrentId, Index) -> {ok,SuccessorId} 
%% Description: given CurrentId and Index returns the minimum Id required by
%% the I'th successor
%%--------------------------------------------------------------------
successor_id(CurrentId, Index) ->
  {ok}.

%%--------------------------------------------------------------------
%% Function: bbsl
%% Description: Binary Shift Left
%% From: http://is.gd/h4Xo
%% I don't really think it works...
%%--------------------------------------------------------------------
bbsl(Bin,Shift) -> 
  <<_:Shift,Rest/bits>> = Bin, <<Rest/bits,0:Shift>>.

%%--------------------------------------------------------------------
%% Function: hex_to_int(HexStr) -> Integer
%% Description: returns an Integer from a well-formed hex string
%% hex_to_int("7b") -> 123 
%%--------------------------------------------------------------------
hex_to_int(HexStr) ->
    {ok, Num, _} = io_lib:fread("~16u", HexStr),
    IntStr = hd(io_lib:format("~B", Num)),
    io:format(user, "Hex: ~p Int: ~p~n", [HexStr, IntStr]),
    list_to_integer(IntStr).


