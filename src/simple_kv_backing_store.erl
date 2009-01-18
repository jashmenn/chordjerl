%%% --------------------------------------------------------------------------
%%% A simple implementation of the gen_ch_backing_store behavior. Its used for
%%% a non-persistant way to test Chord. 
%%%
%%% Obviously taken from Joe's book Programming Erlang
%%% --------------------------------------------------------------------------
-module(simple_kv_backing_store).
-behaviour(gen_ch_backing_store).
-export([start/0, store/2, lookup/1]).

start() -> register(kvs, spawn(fun() -> loop() end)).

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

rpc(Q) ->
    kvs ! {self(), Q},
    receive
        {kvs, Reply} ->
            Reply
    end.

loop() ->
    receive
        {From, {store, Key, Value}} ->
            put(Key, {ok, Value}),
            From ! {kvs, true},
            loop();
        {From, {lookup, Key}} ->
            From ! {kvs, get(Key)},
            loop()
    end.
