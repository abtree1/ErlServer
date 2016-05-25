-module(hexagon).

-define(MAX_SIZE, 2048).

-record(grid, {pointx, pointy, mapid, status, playerid}).

get_neighbor(Grid) ->
	X = Grid#grid.pointx,
	Y = Grid#grid.pointy,
	List = [{X - 1, Y - 2},
	 {X + 1, Y + 2},
	 {X - 2, Y},
	 {X + 2, Y},
	 {X + 1, Y - 2},
	 {X - 1, Y + 2}],
	lists:foldl(fun({M, N}, AccIn) ->
		if
			M < 0 -> AccIn;
			M > ?MAX_SIZE ->AccIn;
			N < 0 -> AccIn;
			N > ?MAX_SIZE ->AccIn; 
			N rem 2 =:= 1 -> AccIn;
			true -> [{M, N}|AccIn]
		end
	end, [], List).

is_line(Grid, OtherGrid) ->
	if 
		Grid#grid.pointy =:= OtherGrid#grid.pointy -> true;
		true ->
			SpanX = erlang:abs(Grid#grid.pointx - OtherGrid#grid.pointx),
			SpanY = erlang:abs(Grid#grid.pointy - OtherGrid#grid.pointy),
			if 
				SpanX * 2 =:= SpanY -> true;
				true -> false
			end
	end.

is_validity(Grid) ->
	if 
		Grid#grid.pointy rem 2 =:= 1 -> false;
		true ->
			T = Grid#grid.pointy div 2,
			if 
				T rem 2 =:= 1 andalso Grid#grid.pointx rem 2 =:= 0 -> true;
				T rem 2 =:= 0 andalso Grid#grid.pointx rem 2 =:= 1 -> true;
				true -> false
			end
	end.