-module(erl_config_file).

-define(CONFIG_DIR, "config").
-define(INCLUDE_DIR, "include/config.hrl").
-define(INCLUDE_DATA_DIR, "include/config_data.hrl").

-export([decompress/0]).

decompress()->
	Accout = filelib:fold_files(?CONFIG_DIR, ".*\.conf", true, fun(Path, AccIn)-> 
		% error_logger:info_msg("erl_config_file：~p~n", [Path]),
		SName = filename:basename(Path, ".conf"),
		Name = list_to_binary(SName),
		case fead_conf_file(binary_to_atom(Name, utf8), Path) of
			{error, _} -> AccIn;
			Columns -> 
				Str = << <<"-record(">>/binary, Name/binary, Columns/binary >>,
				[Str|AccIn]
		end
	end, []),
	% error_logger:info_msg("decompress: ~p~n", [Accout]),
	write_include_hrl(Accout).

fead_conf_file(Name, Path) ->
	case file:read_file(Path) of 
		{error, _} -> {error, {}};
		{ok, Context} -> 
			List = binary_string:split(Context, <<"\r\n">>),
			ConTrim = binary_string:join(List, <<"\n">>), 
			analyse_conf(Name, ConTrim)
	end.

analyse_conf(Name, Context) ->
	[Head|Binaries] = binary_string:split(Context, <<"\n">>),
	%% error_logger:info_msg("analyse_conf:Binaries: ~p~n", [Binaries]),
	{Titles, Types} = analyse_conf_head(Head),
	AccOut = lists:foldl(fun(Data, AccIn) ->
		[Flag|Datas] = binary_string:split(Data, <<"\t">>),
		%% error_logger:info_msg("analyse_conf:Flag: ~p, ~p~n", [Flag, Datas]),
		if 
			Flag =:= <<"#">> -> 
				Res = analyse_conf_data(Datas, Types, []),
				%% error_logger:info_msg("analyse_conf:analyse_conf_data: ~p~n", [Res]),
				[list_to_tuple(Res)|AccIn]; 
			true -> AccIn
		end
	end, [], Binaries),
	%% error_logger:info_msg("analyse_conf: ~p~n", [AccOut]),
	erl_config_store:insert(Name, AccOut),
	Titles.

analyse_conf_head(Head) ->
	[_Flag|Titles] = binary_string:split(Head, <<"\t">>),
	{RName, LType} = lists:foldl(fun(Title, {Names, Types}) ->
		[Name, Type] = binary_string:split(Title, <<":">>),
		{[Name|Names], [Type|Types]}
	end, {[], []}, Titles),
	B = binary_string:join(lists:reverse(RName), <<",">>),
	TName = << <<", {">>/binary, B/binary, <<"}).\r\n">>/binary >>,
	{TName, lists:reverse(LType)}.

analyse_conf_data([], _Types, Result) -> lists:reverse(Result);
analyse_conf_data([Data|Datas], [Type|Types], Result) ->
	%  error_logger:info_msg("analyse_conf_data:~p, ~p, ~p, ~p ~n", [Data, Datas, Type, Types]),
	Value = case Type of 
		<<"integer">> -> binary_to_integer(Data);
		<<"string">> -> Data;
		<<"float">> -> binary_to_float(Data)
	end,
	analyse_conf_data(Datas, Types, [Value|Result]).

write_include_hrl(Records) -> 
	% error_logger:info_msg("write_include_hrl: ~p~n", [Records]),
	file:write_file(?INCLUDE_DIR, Records).
