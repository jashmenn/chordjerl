-module(eunit_simple_kv_backing_store).

-include_lib("eunit/include/eunit.hrl").
-define(MOD, simple_kv_backing_store).

setup() -> 
  erlang:display("setup called"),
  ?MOD:start().

store_and_lookup_test_() ->
  { 
      setup, fun setup/0,
      fun () ->
          ?MOD:store(nate, "at home"),
          ?assertNot(?MOD:lookup(nate) == {ok, "not at home"}),
          ?assertEqual(?MOD:lookup(nate), {ok, "at home"})
      end 
  }.
