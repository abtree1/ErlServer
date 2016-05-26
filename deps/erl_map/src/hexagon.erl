-module(hexagon).

-include("../../include/auto/map_data.hrl").
-include("../../include/properties.hrl").

-export([get_cell/2,
		get_neighbor/1,
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
		X > ?HEX_MAX_SIZE -> false;
		Y > ?HEX_MAX_SIZE -> false;
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
	(SpanX + SpanY div 2) div 2.

get_path(Point, OtherPoint) ->
	get_path(Point, OtherPoint, [], 0).

get_path(Point, Point, Path, _N) -> [Point|Path];
get_path({X, Y}, {OtherX, OtherY}, Path, N) -> 
	NewPath = [{OtherX, OtherY}|Path],
	if 
		Y =:= OtherY ->
			NewX = path_go_y(X, OtherX),
			get_path({X, Y}, {NewX, OtherY}, NewPath, N);
		true ->
			case is_line({X, Y}, {OtherX, OtherY}) of
				true ->
					NewPoint = path_go_x({X, Y}, {OtherX, OtherY}),
					get_path({X, Y}, NewPoint, NewPath, N);
				false ->
					Flag = N rem 2,
					if
						Flag =:= 0 ->
							NewPoint = path_go_x({X, Y}, {OtherX, OtherY}),
							get_path({X, Y}, NewPoint, NewPath, N + 1);
						Flag =:= 1 ->
							NewX = path_go_y(X, OtherX),
							get_path({X, Y}, {NewX, OtherY}, NewPath, N + 1)
					end
			end
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
		true -> OtherX + 1
	end,
	{NewX, NewY}.
