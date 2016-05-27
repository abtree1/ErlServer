-module(rectangle).

-include("../../include/auto/map_data.hrl").
-include("../../include/properties.hrl").

-export([get_cell/2,
		get_neighbor/1,
		get_neighbor/2,
		is_validity/1,
		is_neighbor/2,
		count_space/2,
		is_line/2,
		get_path/2]).

get_cell(X, Y) ->
	Section = X div ?RECT_GROUP_SIZE,
	case lists:keyfind(Section, 1, ?MAPDATAS) of 
		false -> undefined;
		{Section, Groups} ->
			Group = Y div ?RECT_GROUP_SIZE,
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
	List = [{X-1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1}],
	filter_points(List).

get_neighbor(Point, 1) -> get_neighbor(Point);
get_neighbor({X, Y}, L) ->
	Point1 = {X - L, Y},
	Point2 = {X, Y + L},
	Point3 = {X + L, Y},
	Point4 = {X, Y - L},
	Path1 = diagonal_path(Point1, Point2),
	Path2 = diagonal_path(Point2, Point3),
	Path3 = diagonal_path(Point3, Point4),
	Path4 = diagonal_path(Point4, Point1),
	Path = Path1 ++ Path2 ++ Path3 ++ Path4,
	filter_points(Path).

filter_points(List) ->
	lists:foldl(fun(Point, AccIn) ->
		case is_validity(Point) of 
			false -> AccIn;
			true -> [Point|AccIn]
		end
	end, [], List).

diagonal_path(Point1, Point2) ->
	diagonal_path(Point1, Point2, []).

diagonal_path(Point, Point, Path) -> Path;
diagonal_path({X, Y}, {OtherX, OtherY}, Path) ->
	NewX = if 
		X > OtherX -> X - 1;
		X < OtherX -> X + 1
	end,
	NewY = if 
		Y > OtherY -> Y - 1;
		Y < OtherY -> Y + 1
	end,
	diagonal_path({NewX, NewY}, {OtherX, OtherY}, [{X, Y}|Path]).

is_validity({X, Y}) ->
	if 
		X < 0 -> false;
		Y < 0 -> false;
		X >= ?HEX_MAX_SIZE -> false;
		Y >= ?HEX_MAX_SIZE -> false;
		true -> true
	end.

is_neighbor(PointM, PointN) ->
	case count_space(PointM, PointN) of 
		1 -> true;
		_ -> false
	end.

count_space({X, Y}, {OtherX, OtherY}) ->
	SpanX = erlang:abs(X - OtherX),
	SpanY = erlang:abs(Y - OtherY),
	SpanX + SpanY.

is_line({X, Y}, {OtherX, OtherY}) ->
	if 
		X =:= OtherX -> true;
		Y =:= OtherY -> true;
		true -> false
	end.

get_path(Point, OtherPoint) ->
	get_path(Point, OtherPoint, [], 0).

get_path(Point, Point, Path, _N) -> [Point|Path];
get_path({X, Y}, {OtherX, OtherY}, Path, N) -> 
	NewPath = [{OtherX, OtherY}|Path],
	if 
		OtherX =:= X andalso Y > OtherY -> get_path({X, Y}, {OtherX, OtherY + 1}, NewPath, N);
		OtherX =:= X andalso Y < OtherY -> get_path({X, Y}, {OtherX, OtherY - 1}, NewPath, N);
		OtherY =:= Y andalso X > OtherX -> get_path({X, Y}, {OtherX + 1, OtherY}, NewPath, N);
		OtherY =:= Y andalso X < OtherX -> get_path({X, Y}, {OtherX - 1, OtherY}, NewPath, N);
		true ->
			Flag = N rem 2,
			if 
				Flag =:= 0 andalso OtherX > X -> get_path({X, Y}, {OtherX - 1, OtherY}, NewPath, N + 1);
				Flag =:= 0 andalso OtherX < X -> get_path({X, Y}, {OtherX + 1, OtherY}, NewPath, N + 1);
				Flag =:= 1 andalso OtherY > Y -> get_path({X, Y}, {OtherX, OtherY - 1}, NewPath, N + 1);
				Flag =:= 1 andalso OtherY < Y -> get_path({X, Y}, {OtherX, OtherY + 1}, NewPath, N + 1)
			end
	end.


