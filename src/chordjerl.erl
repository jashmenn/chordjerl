-module (chordjerl).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).
-export ([init/1]).
-export ([lookup/1]).

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

lookup(Key) -> chordjerl_srv:lookup(Key).

start(_Type, Config) ->    
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

init([Config]) ->
  ChordjerlServerSup = { chordjerl_srv, {chordjerl_srv,start_link,[Config]}, permanent,2000,worker,[]},
  {ok, {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME}, [ChordjerlServerSup]}}.

stop(State) -> ok.
