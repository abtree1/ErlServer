-module(build_map).

-include("../../include/auto/config.hrl").

-define(HEX_MAX_SIZE, 2048).
-define(RECT_MAX_SIZE, 1024).
% -define(LEVEL_RANK, [{1,20,20}, {2,18,38}, {3,16,54}, {4,13,67}, {5,11,78}, {6,9,87}, {7,7,94}, {8,4,98}, {9,2,100}]).
-define(GROUP_SIZE, 100).

-export([build/1]).

build(Type) when Type =:= hexagon -> build_hexagon();
build(Type) when Type =:= hex -> build_hexagon();
build(Type) when Type =:= rectangle -> build_rectangle();
build(Type) when Type =:= rect -> build_rectangle().

build_hexagon() -> 
	%% Grids = build_point(1, 0, []),
	application:start(erl_config),
	random:seed(os:timestamp()),
	{GroupSize, RankLevel} = init_level_rank(),
	Grids = inset_grid(RankLevel, GroupSize, [], 1),
	error_logger:info_msg("~p~n", [Grids]),
	application:stop(erl_config).

build_point(_X, ?HEX_MAX_SIZE, Grids) -> lists:reverse(Grids);	
build_point(X, Y, Grids) ->
	if 
		X + 2 < ?HEX_MAX_SIZE -> build_point(X + 2, Y, [{X, Y}|Grids]);
		true -> 
			NewX = (X + 1) rem 2,
			build_point(NewX, Y + 2, [{X, Y}|Grids])
	end.

inset_grid(_LevelRank, 0, Grids, _N) -> lists:reverse(Grids);
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

choose_level(Hit, [{Level, Amount, Weight}|LevelRank]) ->
	if
		Hit =< Weight -> {Level, Amount, Weight};
		true -> choose_level(Hit, LevelRank)
	end.

init_level_rank() ->
	Maps = erl_config:all(map),
	lists:foldl(fun(Map, {Total, OldLevelRank}) ->
		NewTotal = Total + Map#map.amount,
		NewLevelRank = [{Map#map.id, Map#map.amount, NewTotal}|OldLevelRank],
		{NewTotal, NewLevelRank}
	end, {0, []}, Maps).

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

build_rectangle() -> ok.