% Includes file for the Chordjerl
-define (DEBUG, true).

-define (DICT, dict).
-define (TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n",           [?MODULE, ?LINE, X, M])).
-define (NTRACE(X, M), io:format(user, "NTRACE ~p:~p ~p ~p ~p ~p ~p~n", [?MODULE, ?LINE, chordjerl_srv:registered_name(), node(), self(), X, M])).
-define (RECONNECT_TIMEOUT, 10000).
-define (MAXFINGERS, ?NBIT). % number of bits in the hash function
-define (NBITMOD, round(math:pow(2, ?NBIT))).

-ifdef (DEBUG).
-define (NBIT, 7). % number of bits in the hash function
-else.
-define (NBIT, 160). % number of bits in the hash function
-endif.

-define (DEFAULT_CONFIG, []).

% may merge finger-like attributes into the srv state. Maybe
% srv_state.finger_of_self?
-record(srv_state, {
    fingers = [],
    predecessor,
    tref,
    backing_store = simple_kv_backing_store,
    sha,
    pid,
    ip,
    port,
    next % the index of the next finger to fix
  }).

-record(finger, {
    sha,
    node,
    pid,
    ip,
    port
  }).

