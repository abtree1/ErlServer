-module(erl_config_dirtywords).

-include("../../include/properties.hrl").

-export([decompress/0]).

decompress() ->
	case file:read_file(?CONFIG_DIRTY_WORDS) of 
		{error, _} -> error;
		{ok, Context} ->
			List = binary_string:split(Context, <<"\r\n">>),
			ConTrim = binary_string:join(List, <<"\n">>), 
			analyse_conf(ConTrim)
	end.

analyse_conf(Context) ->
	Lists = binary_string:split(Context, <<"\n">>),
	AccOut = lists:foldl(fun(Bin, AccIn)->
		Str = binary_to_list(Bin),
		analyse_string(Str, AccIn)
	end, [], Lists),
	%% error_logger:info_msg("analyse_conf:~n~p~n", [AccOut]),
	write_hrl(AccOut).
 
analyse_string([], _Result) -> stop;
analyse_string([Data|Datas], Result) ->
	Ret = case lists:keyfind(Data, 1, Result) of 
		false -> analyse_string(Datas, []);
		{Data, List} -> analyse_string(Datas, List)
	end,
	Ret1 = {Data, Ret},
	lists:keystore(Data, 1, Result, Ret1).

write_hrl(List) ->
	AccOut = lists:foldl(fun(Item, AccIn) ->
		B = pass_to_bin(Item),
		[B|AccIn]
	end, [], List),
	Bin = binary_string:join(AccOut, <<",">>),
	File = << <<"-define(DIRTYWORDS, [">>/binary, Bin/binary, <<"]).">>/binary >>,
	file:write_file(?CONFIG_INCLUDE_DIRTY_WORDS, File).

pass_to_bin({Data, stop}) ->
	<< <<"{<<\"">>/binary,<<Data>>/binary,<<"\">>,stop}">>/binary >>;
pass_to_bin({Data, Tails}) ->
	AccOut = lists:foldl(fun(Item, AccIn) ->
		Bin = pass_to_bin(Item),
		[Bin|AccIn]
	end, [], Tails),
	B = binary_string:join(AccOut, <<",">>),
	B1 = << <<"[">>/binary,B/binary,<<"]">>/binary >>,
	<< <<"{<<\"">>/binary,<<Data>>/binary,<<"\">>,">>/binary,B1/binary,<<"}">>/binary >>.
