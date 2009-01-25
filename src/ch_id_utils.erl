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
%%--------------------------------------------------------------------
bbsl(Bin,Shift) -> 
  <<_:Shift,Rest/bits>> = Bin, <<Rest/bits,0:Shift>>.
