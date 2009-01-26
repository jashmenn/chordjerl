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

bin_shift_left_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         Base  = <<1>>,
         Shift = 1,

         Shifted = ch_id_utils:bbsl(Base, Shift)
         % io:format(user, "~p~n", [Shifted]),
         % BitString = bin_to_bitstring(Shifted),
         %io:format(user, "~p~n", [BitString])
         %ShiftedInBits = hd(io_lib:format("~.2B", [Shifted])),
         %io:fwrite("~p~n", [ShiftedInBits])
      end
  }.


