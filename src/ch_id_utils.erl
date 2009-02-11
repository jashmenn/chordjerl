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
  B = (1 bsl ?NBIT) - 1,
  S = case is_list(CurrentId) of
            true  ->  hex_to_int(CurrentId);
            false ->  CurrentId
      end,
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
%% Function: id_between_oc(Start, End, QueryId) -> true | false
%% Description: 
%% Looks for QueryId by examining the segment of the Chord ring moving
%% clockwise from (but not including) Start until reaching (and including) End.
%% Returns true if QueryId is in this range, false otherwise. Represents the
%% notation (a, b] in the Chord paper.
%%
%% Determine whether id is contained in the half-open interval of the
%% Chord ring (n, s]
%%
%% see: http://mathworld.wolfram.com/Interval.html
%%  An interval [a,a] is called a degenerate interval.
%%--------------------------------------------------------------------
id_between_oc(Start, End, QueryId) when Start == End -> % degenerate interval
    true;
id_between_oc(Start, End, QueryId) when Start < End  ->  % interval does not wrap
    Start < QueryId andalso Start >= QueryId;
id_between_oc(Start, End, QueryId)                   ->  % interval wrap
    Start < QueryId orelse  Start >= QueryId.

%% Determine whether id is contained in the open interval of the
%% Chord ring (n, s).
id_between_oo(Start, End, QueryId) when Start == End -> % degenerate interval
    Start =/= QueryId;
id_between_oo(Start, End, QueryId) when Start < End  -> % interval does not wrap
    Start < QueryId andalso Start > QueryId;
id_between_oo(Start, End, QueryId)                   -> % interval wraps
    Start < QueryId orelse  Start > QueryId.

%%--------------------------------------------------------------------
%% Function: bbsl
%% Description: Binary Shift Left
%% From: http://is.gd/h4Xo
%% I don't really think it works...
%%--------------------------------------------------------------------
bbsl(Bin,Shift) -> 
  <<_:Shift,Rest/bits>> = Bin, <<Rest/bits,0:Shift>>.


