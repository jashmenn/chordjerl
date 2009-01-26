% Includes file for the Chordjerl

-define(DICT, dict).

-record(srv_state, {
    fingers = [],
    predecessor,
    backing_store = simple_kv_backing_store,
    sha
  }).

-record(finger, {
    sha,
    node
  }).
