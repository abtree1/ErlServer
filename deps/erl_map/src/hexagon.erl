-module(hexagon).

-include("../../include/auto/map_data.hrl").
-include("../../include/properties.hrl").

-export([get_cell/2,
		get_neighbor/1,
		get_neighbor/2,
		is_neighbor/2,
		is_line/2,
		is_validity/1,
		count_space/2,
		get_path/2]).

get_cell(X, Y) ->
	Section = X div ?HEX_GROUP_SIZE,
	case lists:keyfind(Section, 1, ?MAPDATAS) of 
		false -> undefined;
		{Section, Groups} ->
			Group = Y div ?HEX_GROUP_SIZE,
			case lists:keyfind(Group, 1, Groups) of 
				false -> undefined;
				{Group, Cells} ->
					case lists:keyfind({X, Y}, 1, Cells) of 
						false -> undefined;
						Cell -> Cell 
					end
			end
	end. 

get_neighbor({X, Y}) ->
	List = [{X - 1, Y - 2},
	 {X + 1, Y + 2},
	 {X - 2, Y},
	 {X + 2, Y},
	 {X + 1, Y - 2},
	 {X - 1, Y + 2}],
	filter_points(List).

get_neighbor(Point, 1) -> get_neighbor(Point);
get_neighbor({X, Y}, L) ->
	Point1 = {X - L, Y - 2*L},
	Point2 = {X + L, Y - 2*L},
	Point3 = {X + 2*L, Y},
	Point4 = {X + L, Y + 2*L},
	Point5 = {X - L, Y + 2*L},
	Point6 = {X - 2*L, Y},
	Path1 = get_path_over(Point1, Point2, []),
	Path2 = get_path_over(Point2, Point3),
	Path3 = get_path_over(Point3, Point4, []),
	Path4 = get_path_over(Point4, Point5),
	Path5 = get_path_over(Point5, Point6, []),
	Path6 = get_path_over(Point6, Point1),
	Path = Path1 ++ Path2 ++ Path3 ++ Path4 ++ Path5 ++ Path6,
	filter_points(Path).

filter_points(List) ->
	lists:foldl(fun(Point, AccIn) ->
		case is_validity(Point) of 
			false -> AccIn;
			true -> [Point|AccIn]
		end
	end, [], List).

is_neighbor(PointM, PointN) ->
	case count_space(PointM, PointN) of 
		1 -> true;
		_ -> false
	end.

is_line({X, Y}, {OtherX, OtherY}) ->
	if 
		Y =:= OtherY -> true;
		true ->
			SpanX = erlang:abs(X - OtherX),
			SpanY = erlang:abs(Y - OtherY),
			if 
				SpanX * 2 =:= SpanY -> true;
				true -> false
			end
	end.

is_validity({X, Y}) ->
	if 
		X < 0 -> false;
		Y < 0 -> false;
		X >= ?HEX_MAX_SIZE -> false;
		Y >= ?HEX_MAX_SIZE -> false;
		Y rem 2 =:= 1 -> false;
		true ->
			T = Y div 2,
			if 
				T rem 2 =:= 1 andalso X rem 2 =:= 0 -> true;
				T rem 2 =:= 0 andalso X rem 2 =:= 1 -> true;
				true -> false
			end
	end.

count_space({X, Y}, {OtherX, OtherY}) ->
	SpanX = erlang:abs(X - OtherX),
	SpanY = erlang:abs(Y - OtherY),
	Span = SpanX + SpanY div 2,
	if
		X =:= OtherX -> Span;
		true -> Span div 2
	end.

get_path(Point, OtherPoint) ->
	get_path(Point, OtherPoint, []).

get_path(Point, Point, Path) -> [Point|Path];
get_path({X, Y}, {OtherX, OtherY}, Path) -> 
	NewPath = [{OtherX, OtherY}|Path],
	if 
		Y =:= OtherY ->
			NewX = path_go_y(X, OtherX),
			get_path({X, Y}, {NewX, OtherY}, NewPath);
		true ->
			NewPoint = path_go_x({X, Y}, {OtherX, OtherY}),
			get_path({X, Y}, NewPoint, NewPath)
	end. 

get_path_over(Point, OtherPoint) -> 
	[_|List] = get_path_over(Point, OtherPoint, []), 
	lists:droplast(List).

get_path_over(Point, Point, Path) -> [Point|Path];
get_path_over({X, Y}, {OtherX, OtherY}, Path) ->
	NewPath = [{OtherX, OtherY}|Path],
	if 
		Y =:= OtherY ->
			NewX = path_go_y(X, OtherX),
			get_path_over({X, Y}, {NewX, OtherY}, NewPath);
		true ->
			NewPoint = path_go_x_over({X, Y}, {OtherX, OtherY}),
			get_path_over({X, Y}, NewPoint, NewPath)
	end. 

path_go_y(X, OtherX) ->
	if 
		X < OtherX -> OtherX - 2;
		true -> OtherX + 2
	end.

path_go_x({X, Y}, {OtherX, OtherY}) ->
	NewY = if 
		OtherY < Y -> OtherY + 2;
		OtherY > Y -> OtherY - 2
	end,
	NewX = if 
		X < OtherX -> OtherX - 1;
		OtherX + 1 >= ?HEX_MAX_SIZE -> OtherX - 1;
		true -> OtherX + 1
	end,
	{NewX, NewY}.

path_go_x_over({X, Y}, {OtherX, OtherY}) ->
	NewY = if 
		OtherY < Y -> OtherY + 2;
		OtherY > Y -> OtherY - 2
	end,
	NewX = if 
		X < OtherX -> OtherX - 1;
		true -> OtherX + 1
	end,
	{NewX, NewY}.

