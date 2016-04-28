-module(uuid_factory).

-export([get_uuid/0]).

%% uuid = timestamp + random(1 ~ 9) + counter(auto_increment) 16bit binary
get_uuid() ->
	Now = time_utils:now(),
	NowB = integer_to_binary(Now),
	Count = erl_counter:get_daily_action(uuid_factory),
	erl_counter:incr_daily_action(uuid_factory, 1),
	{Head, Tail} = binary_count(Count),
	R = random:uniform(9),
	RB = integer_to_binary(R),
	<< NowB:10/binary, RB:1/binary, Head/binary, Tail/binary >>.

binary_count(Count) ->
	if
		Count < 10 -> {<<"0000">>, integer_to_binary(Count)};
		Count < 100 -> {<<"000">>, integer_to_binary(Count)};
		Count < 1000 -> {<<"00">>, integer_to_binary(Count)};
		Count < 10000 -> {<<"0">>, integer_to_binary(Count)};
		true -> {<<"">>, integer_to_binary(Count)}
	end.