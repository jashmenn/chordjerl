-module(eunit_ch_id_utils).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok}.

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

hex_to_int_test_() ->
  {
      setup, fun setup/0,
      fun () ->
        Values = [ {"1",    1},
                   {"a",    10},
                   {"64",   100},
                   {"3e8",  1000},
                   {"2710", 10000},
                   {"7b",   123},
                   {"955",  2389},
                   {"16e7e5d52b", 98379879723}],

        [ ?assertEqual(Int, ch_id_utils:hex_to_int(Hex)) || {Hex, Int} <- Values ]
      end
  }.

successor_id_test_() ->
  {
      setup, fun setup/0,
      fun () ->
        Values = [ 
                   % CurrentId     % Index    % Wanted
                                              % todo, verify the math of the wanted values
                   {[1,            1],         3},
                   {["1",          1],         3},
                   {["a",          2],        14},
                   {["64",         2],       104},
                   {["3e8",        3],      1008},
                   {["2710",       3],     10008},
                   {["7b",         4],       139},
                   {["955",        4],      2405},
                   {["16e7e5d52b", 5],     98379879755}],
               
        [ ?assertEqual(Wanted, ch_id_utils:successor_id(CurrentId, Index)) || {[CurrentId, Index], Wanted} <- Values ]
      end
  }.

id_between_oc_test_() ->
  {
      setup, fun setup/0,
      fun () ->
        Values = [ 
            % {Start, End, QueryId, Wanted}
              {0,      10,       5,  true},
              {0,      10,      10,  true},
              {5,      10,       5, false},
              {5,      10,       6,  true},
              {100,    1,        6, false},
              {100,    1,      101,  true}, % wrapping
              {100,    1,       50, false}, 
              {1,      1,        1,  true},
              {1,      1,        2,  true}, % eh?
              {1,      1,       50,  true}, 
              {112,   59,       36,  true} 
         ],
               
        [ ?assertEqual(Wanted, ch_id_utils:id_between_oc(Start, End, QueryId)) || {Start, End, QueryId, Wanted} <- Values ]
      end
  }.

id_between_oo_test_() ->
  {
      setup, fun setup/0,
      fun () ->
        Values = [ 
            % {Start, End, QueryId, Wanted}
              {0,      10,       5,  true},
              {0,      10,      10, false},
              {5,      10,       5, false},
              {5,      10,       6,  true},
              {100,    1,        6, false},
              {100,    1,      101,  true}, 
              {100,    1,       50, false}, 
              {1,      1,        1, false},
              {1,      1,        2,  true},
              {112,   59,       36,  true} 
         ],
               
        [ ?assertEqual(Wanted, ch_id_utils:id_between_oo(Start, End, QueryId)) || {Start, End, QueryId, Wanted} <- Values ]
      end
  }.
