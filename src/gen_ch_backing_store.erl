-module(gen_ch_backing_store).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{lookup, 1}, {store,2}];
behaviour_info(_) ->
    undefined.
