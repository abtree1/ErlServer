-module(build_rectangle).

-include("../../include/properties.hrl").

-export([build/0]).

build() ->
	random:seed(os:timestamp()),
	{GroupSize, RankLevel} = build_map:init_level_rank(),
	Sections = build_section({GroupSize, RankLevel}, {0, 0, ?RECT_GROUP_SIZE, ?RECT_GROUP_SIZE}, []),
	build_map:write_file(Sections).

build_section(_, {_X, _Y, ?RECT_MAX_SIZE + ?RECT_GROUP_SIZE, _}, Sections) -> Sections;
build_section({GroupSize, RankLevel}, {X, Y, SecSize, HexSize}, Sections) ->
	Groups = build_group({GroupSize, RankLevel}, {X, Y, SecSize, HexSize}, []),
	BinGroups = binary_string:join(Groups, <<",\r\n">>),
	Bin = << <<", [">>/binary, BinGroups/binary, <<"]">>/binary >>,
	Index = SecSize div ?RECT_GROUP_SIZE - 1,
	BinIndex = integer_to_binary(Index),
	BinSection = << <<"{">>/binary, BinIndex/binary, Bin/binary, <<"}">>/binary >>,
	build_section({GroupSize, RankLevel}, {SecSize + 1, Y, SecSize + ?RECT_GROUP_SIZE, HexSize}, [BinSection|Sections]).

build_group(_, {_X, _Y, _, ?RECT_MAX_SIZE + ?RECT_GROUP_SIZE}, Groups) -> Groups;
build_group({GroupSize, RankLevel}, {X, Y, SecSize, HexSize}, Groups) ->
	Grids = build_map:inset_grid(RankLevel, GroupSize, [], 1),
	Points = build_point(X, Y, SecSize, HexSize, []),
	Records = build_map:integration_list(Points, Grids, []),
	Group = build_map:pass_to_binary(Records),
	Index = HexSize div ?RECT_GROUP_SIZE - 1, 
	BinIndex = integer_to_binary(Index),
	BinGroup = << <<"{">>/binary, BinIndex/binary, <<", ">>/binary, Group/binary, <<"}">>/binary >>,
	build_group({GroupSize, RankLevel}, {X, HexSize, SecSize, HexSize + ?RECT_GROUP_SIZE}, [BinGroup|Groups]).

build_point(_X, HexSize, _SecSize, HexSize, Grids) -> Grids; 	
build_point(X, Y, SecSize, HexSize, Grids) ->
	if 
		X + 1 < SecSize -> build_point(X + 1, Y, SecSize, HexSize, [{X, Y}|Grids]);
		true -> 
			NewX = SecSize - ?RECT_GROUP_SIZE,
			build_point(NewX, Y + 1, SecSize, HexSize, [{X, Y}|Grids])
	end.


