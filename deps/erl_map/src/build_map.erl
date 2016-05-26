-module(build_map).

-include("../../include/auto/config.hrl").
-include("../../include/properties.hrl").

-export([build/0, build/1]).
-export([init_level_rank/0, inset_grid/4, integration_list/3, pass_to_binary/1, write_file/1]).

build() ->
	build(?MAP_TYPE).

build(Type) when Type =:= hexagon -> build_hexagon:build();
build(Type) when Type =:= hex -> build_hexagon:build();
build(Type) when Type =:= rectangle -> build_rectangle:build();
build(Type) when Type =:= rect -> build_rectangle:build().

init_level_rank() ->
	Maps = erl_config:all(map),
	lists:foldl(fun(Map, {Total, OldLevelRank}) ->
		NewTotal = Total + Map#map.amount,
		NewLevelRank = [{Map#map.id, Map#map.amount, NewTotal}|OldLevelRank],
		{NewTotal, NewLevelRank}
	end, {0, []}, Maps).

inset_grid(_LevelRank, 0, Grids, _N) -> Grids; %% lists:reverse(Grids);
inset_grid(LevelRank, Total, Grids, N) ->
	Hit = random:uniform(Total),
	{Level, Amount, Weight} = choose_level(Hit, LevelRank),
	NewGrids = [{N, Level}|Grids],
	NewLevelRank = if 
		Total > 1 -> 
			TLevelRank = lists:keyreplace(Level, 1, LevelRank, {Level, Amount - 1, Weight}),
			{_Total, LR} = build_level_rank(TLevelRank, 0, []),
			LR;
		true -> LevelRank
	end,
	inset_grid(NewLevelRank, Total - 1, NewGrids, N + 1).

build_level_rank([], Total, RankList) -> {Total, lists:reverse(RankList)};
build_level_rank([{Level, Amount, _}|List], Total, RankList) ->
	{NewTotal, NewRankList} = if 
		Amount > 0 ->
			NTotal = Total + Amount,
			NRankList = [{Level, Amount, NTotal}|RankList],
			{NTotal, NRankList};
		true -> {Total, RankList}
	end,
	build_level_rank(List, NewTotal, NewRankList).

choose_level(Hit, [{Level, Amount, Weight}|LevelRank]) ->
	if
		Hit =< Weight -> {Level, Amount, Weight};
		true -> choose_level(Hit, LevelRank)
	end.

integration_list([], _, Records) -> Records;
integration_list([Point|Points], [{_N, LevelId}|Grids], Records) ->
	NewRecords = [{Point, LevelId}|Records],
	integration_list(Points, Grids, NewRecords).

pass_to_binary(Records) ->
	BinRecords = lists:foldl(fun({{X, Y}, Id}, AccIn) ->
		BinX = integer_to_binary(X),
		BinY = integer_to_binary(Y),
		BinId = integer_to_binary(Id),
		Bin = << <<"{{">>/binary, BinX/binary, <<", ">>/binary, BinY/binary, <<"}, ">>/binary, BinId/binary, <<"}">>/binary >>,
		[Bin|AccIn]
	end, [], Records),
	Bin = binary_string:join(BinRecords, <<",">>),
	<< <<"[">>/binary, Bin/binary, <<"]">>/binary >>.

write_file(Sections) ->
	Bin = binary_string:join(Sections, <<",\r\n">>),
	File = << <<"-define(MAPDATAS, [">>/binary, Bin/binary, <<"]).">>/binary >>,
	file:write_file(?MAP_INCLUDE_DATA, File).