-module(eunit_ch_id_utils).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok}.


bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

bin_to_bitstring(Bin) ->
  lists:flatten([io_lib:format("~.2B", [X]) || X <- binary_to_list(Bin)]).

bin_shift_left_test_() ->
  {
      setup, fun setup/0,
      fun () ->

         Base  = <<1>>,
         Shift = 9,

         Shifted = ch_id_utils:bbsl(Base, Shift),
         % io:format(user, "~p~n", [Shifted]),

         BitString = bin_to_bitstring(Shifted),
         io:format(user, "~p~n", [BitString])

         %ShiftedInBits = hd(io_lib:format("~.2B", [Shifted])),
         %io:fwrite("~p~n", [ShiftedInBits])

         %?assertEqual(, ),
         %hd(io_lib:format("~.2B", [16])).

         %?assertEqual(ch_id_utils:bbsl(<<1>>, 0), <<1>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 1), <<2>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 2), <<4>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 3), <<8>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 4), <<16>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 5), <<32>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 6), <<64>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 7), <<128>>),
         %?assertEqual(ch_id_utils:bbsl(<<1>>, 8), <<256>>),
    %     ?assertEqual(ch_id_utils:bbsl(<<1>>, 9), <<512>>)
      end
  }.


