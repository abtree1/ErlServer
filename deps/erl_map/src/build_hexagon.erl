-module(build_hexagon).

-include("../../include/properties.hrl").

-export([build/0]).

build() -> 
	random:seed(os:timestamp()),
	{GroupSize, RankLevel} = build_map:init_level_rank(),
	Sections = build_section({GroupSize, RankLevel}, {1, 0, ?HEX_GROUP_SIZE, ?HEX_GROUP_SIZE}, []),
	build_map:write_file(Sections).

build_section(_, {_X, _Y, ?HEX_MAX_SIZE + ?HEX_GROUP_SIZE, _}, Sections) -> Sections;
build_section({GroupSize, RankLevel}, {X, Y, SecSize, HexSize}, Sections) ->
	Groups = build_group({GroupSize, RankLevel}, {X, Y, SecSize, HexSize}, []),
	BinGroups = binary_string:join(Groups, <<",\r\n">>),
	Bin = << <<", [">>/binary, BinGroups/binary, <<"]">>/binary >>,
	Index = SecSize div ?HEX_GROUP_SIZE - 1,
	BinIndex = integer_to_binary(Index),
	BinSection = << <<"{">>/binary, BinIndex/binary, Bin/binary, <<"}">>/binary >>,
	build_section({GroupSize, RankLevel}, {SecSize + 1, Y, SecSize + ?HEX_GROUP_SIZE, HexSize}, [BinSection|Sections]).

build_group(_, {_X, _Y, _, ?HEX_MAX_SIZE + ?HEX_GROUP_SIZE}, Groups) -> Groups;
build_group({GroupSize, RankLevel}, {X, Y, SecSize, HexSize}, Groups) ->
	Grids = build_map:inset_grid(RankLevel, GroupSize, [], 1),
	Points = build_point(X, Y, SecSize, HexSize, []),
	Records = build_map:integration_list(Points, Grids, []),
	Group = build_map:pass_to_binary(Records),
	Index = HexSize div ?HEX_GROUP_SIZE - 1, 
	BinIndex = integer_to_binary(Index),
	BinGroup = << <<"{">>/binary, BinIndex/binary, <<", ">>/binary, Group/binary, <<"}">>/binary >>,
	build_group({GroupSize, RankLevel}, {X, HexSize, SecSize, HexSize + ?HEX_GROUP_SIZE}, [BinGroup|Groups]).

build_point(_X, HexSize, _SecSize, HexSize, Grids) -> Grids; %% lists:reverse(Grids);	
build_point(X, Y, SecSize, HexSize, Grids) ->
	if 
		X + 2 < SecSize -> build_point(X + 2, Y, SecSize, HexSize, [{X, Y}|Grids]);
		true -> 
			NewX = ((X + 1) rem 2) + SecSize - ?HEX_GROUP_SIZE,
			build_point(NewX, Y + 2, SecSize, HexSize, [{X, Y}|Grids])
	end.