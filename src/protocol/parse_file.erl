-module(parse_file).

-include("../../include/properties.hrl").

-export([parse_protocol/0, parse_controller/0]).

parse_protocol() ->
	case file:read_file(?PROTOCOL_UTIL_FILE) of 
		{error, _} -> error;
		{ok, Context} ->
			List = binary_string:split(Context, <<"\r\n">>),
			ConTrim = binary_string:join(List, <<"\n">>), 
			analyse_protocol(ConTrim)
	end.

analyse_protocol(Context) ->
	List = binary_string:split(Context, <<"\n">>),
	put(id, []),
	AccOut = lists:foldl(fun(Line, AccIn) ->
		LLine = parse_annotation(Line),
		if
			LLine =:= <<"">> -> AccIn;
			true -> 
				Res = parse_type(LLine),
				[Res|AccIn]
		end
	end, [], List),
	Ids = get(id),
	erase(id),
	%% error_logger:info_msg("ids, ~p~n", [Ids]),
	write_protocol(AccOut, Ids).

parse_annotation(Line) ->
	[C|_Tail] = binary_string:split(Line, <<"#">>),
	C.

parse_type(Line) ->
	F = binary:at(Line, 0),
	if
		<<F>> =:= <<"\t">> -> parse_body(Line, <<"\t">>); 
		<<F>> =:= <<" ">> -> parse_body(Line, <<" ">>);
		true -> parse_title(Line)
	end.

parse_title(Line) ->
	[Title, Id] = binary_string:split(Line, <<":">>),
	if
		Id =:= <<"">> -> {title, Id, Title};
		true ->
			I = binary_to_integer(Id),
			List = get(id),
			put(id, [I|List]),
			{title, Id, Title}
	end.

parse_body(Line, Char) ->
	Line1 = binary:replace(Line, Char, <<"">>, [global]),
	if
		Line1 =:= <<"">> -> {empty};
		true ->
			[Name, Type] = binary_string:split(Line1, <<":">>),
			NType = case binary_string:split(Type, <<"-">>) of
				%% [<<"array">>, T] -> << <<"{array, ">>/binary, T/binary, <<"}">>/binary >>;
				[<<"array">>, T] -> {<<"array">>, T};
				_ -> Type 
			end,
			{cell, Name, NType}
	end.

write_protocol(Lists, Ids) ->
	{IdStr, NameStr, Res} = analyse_write(Lists, Ids, 1, [], [], [], []),
	BinIds = binary_idstr(IdStr),
	BinNames = binary_namestr(NameStr),
	%% error_logger:info_msg("XXXXXXXXXXXXXXXXXXXX~n~p~nXXXXXXXXXXXXXXXXXXXXX~n", [Res]),
	NRes = type_simple(Res),
	%% error_logger:info_msg("XXXXXXXXXXXXXXXXXXXX~n~p~nXXXXXXXXXXXXXXXXXXXXX~n", [NRes]),
	BinRes = binary_res(NRes),
	%% error_logger:info_msg("XXXXXXXXXXXXXXXXXXXX~n~p~nXXXXXXXXXXXXXXXXXXXXX~n", [BinRes]),
	file:write_file(?PROTOCOL_ENUM, [BinIds, BinNames]),
	file:write_file(?PROTOCOL_RULES, BinRes).

analyse_write([], _Ids, _Id, IdStr, NameStr, Res, _Cell) -> {IdStr, NameStr, Res};
analyse_write([{empty}|Lists], Ids, Id, IdStr, NameStr, Res, Cell) ->
	 analyse_write(Lists, Ids, Id, IdStr, NameStr, Res, Cell);
analyse_write([{cell, _Name, Type}|Lists], Ids, Id, IdStr, NameStr, Res, Cell) ->
	 analyse_write(Lists, Ids, Id, IdStr, NameStr, Res, [Type|Cell]);
analyse_write([{title, IdS, Title}|Lists], Ids, Id, IdStr, NameStr, Res, Cell) ->
	{NidS, Nid, NIds} = if 
		IdS =:= <<"">> ->
			Uid = get_id(Ids, Id),
			{integer_to_binary(Uid), Uid, [Uid|Ids]};
		true -> {IdS, Id, Ids}
	end,
	IdBin = << <<"{">>/binary, NidS/binary, <<", ">>/binary, Title/binary, <<"}">>/binary >>,
	NameBin = << <<"{">>/binary, Title/binary, <<", ">>/binary, NidS/binary, <<"}">>/binary >>,
	%% Cells = binary_string:join(Cell, <<", ">>),
	%% NRes = << <<"{">>/binary, Title/binary, <<", {">>/binary, Cells/binary, <<"}}">>/binary >>,
	analyse_write(Lists, NIds, Nid, [IdBin|IdStr], [NameBin|NameStr], [{Title, Cell}|Res], []).

get_id(Ids, Id) ->
	case lists:member(Id, Ids) of 
		true -> get_id(Ids, Id + 1);
		false -> Id
	end.

type_simple(Lists) ->
	type_simple(Lists, Lists, []).

type_simple([], _Olds, Res) -> Res;
type_simple([{Title, Cells}|Lists], Olds, Res) ->
	List = type_simple_cells(Cells, Olds),
	type_simple(Lists, Olds, [{Title, List}|Res]).

type_simple_cells(Cells, Lists) ->
	lists:foldl(fun(Cell, AccIn) ->
		NType = case Cell of 
			{<<"array">>, T} -> 
				Type = type_simple_cell_do(T, Lists),
				[Type, <<"array">>];
			T ->
				case lists:keyfind(T, 1, Lists) of
					false -> T;
					{_, Type} -> type_simple_cells(Type, Lists)
				end
		end,
		[NType|AccIn]
	end, [], Cells).

type_simple_cell_do(T, Lists) ->
	case lists:keyfind(T, 1, Lists) of 
		false -> T;
		{_, Cells} -> type_simple_cells(Cells, Lists)
	end.

binary_idstr(Ids) ->
	Bin = binary_string:join(Ids, <<", ">>),
	<< <<"-define(PROTOIDENUM, [">>/binary, Bin/binary, <<"]). \r\n">>/binary >>.

binary_namestr(Names) ->
	Bin = binary_string:join(Names, <<", ">>),
	<< <<"-define(PROTONAMEENUM, [">>/binary, Bin/binary, <<"]). \r\n">>/binary >>.

binary_res(Res) ->
	AccOut = lists:foldl(fun({Title, Cells}, AccIn) ->
		Bin = binary_cell(Cells),
		Item = << <<"{">>/binary, Title/binary, <<", ">>/binary, Bin/binary, <<"}">>/binary >>,
		[Item|AccIn]
	end, [], Res),
	Bin1 = binary_string:join(AccOut, <<",">>),
	<< <<"-define(PROTORULE, [">>/binary, Bin1/binary, <<"]). \r\n">>/binary >>.

binary_cell(Cells) ->
	AccOut = lists:foldl(fun(Cell, AccIn) ->
		if 
			is_binary(Cell) -> [Cell|AccIn];
			true ->  
				CellB = binary_cell(Cell),
				[CellB|AccIn]
		end
	end, [], Cells),
	case AccOut of
		[] -> <<"{}">>;
		[<<"array">>|Tails] ->
			Bin1 = binary_string:join(Tails, <<",">>),
			<< <<"[">>/binary, Bin1/binary, <<"]">>/binary >>;
		_ ->
			Bin1 = binary_string:join(AccOut, <<",">>),
			<< <<"{">>/binary, Bin1/binary, <<"}">>/binary >>
	end.	

parse_controller() ->
	case file:read_file(?PROTOCOL_CONTROLLER_FILE) of 
		{error, _} -> error;
		{ok, Context} ->
			List = binary_string:split(Context, <<"\r\n">>),
			ConTrim = binary_string:join(List, <<"\n">>),
			analyse_controller(ConTrim)
	end.

analyse_controller(Context) ->
	List = binary_string:split(Context, <<"\n">>),
	AccOut = lists:foldl(fun(Line, AccIn) ->
		LLine = parse_annotation(Line),
		if
			LLine =:= <<"">> -> AccIn;
			true -> 
				Res = parse_line(LLine),
				[Res|AccIn]
		end
	end, [], List),
	Bins = binary_co(AccOut, [], {}),
	write_controller(Bins).

parse_line(Line) ->
	F = binary:at(Line, 0),
	if
		<<F>> =:= <<"\t">> -> parse_co(Line, <<"\t">>); 
		<<F>> =:= <<" ">> -> parse_co(Line, <<" ">>);
		true -> 
			[Title|_] = binary_string:split(Line, <<":">>),
			{title, Title}
	end.

parse_co(Line, Char) ->
	Line1 = binary:replace(Line, Char, <<"">>, [global]),
	if
		Line1 =:= <<"">> -> {empty};
		true ->
			[Module, Fun] = binary_string:split(Line1, <<":">>),
			{cell, Module, Fun}
	end.

binary_co([], Res, _Cells) -> Res;
binary_co([{empty}|List], Res, Cells) ->
	binary_co(List, Res, Cells);
binary_co([{cell, Module, Fun}|List], Res, _Cells) ->
	binary_co(List, Res, {Module, Fun});
binary_co([{title, Title}|List], Res, {Module, Fun}) ->
	Bin = << <<"{">>/binary, Title/binary, <<", ">>/binary, Module/binary, <<", ">>/binary, Fun/binary, <<"}">>/binary >>,
	binary_co(List, [Bin|Res], {}).

write_controller(Bins) ->
	Bin = binary_string:join(Bins, <<", ">>),
	Stream = << <<"-define(PROTOCONTROLLER, [">>/binary, Bin/binary, <<"]). \r\n">>/binary >>,
	file:write_file(?PROTOCOL_CONTROLLER, Stream).