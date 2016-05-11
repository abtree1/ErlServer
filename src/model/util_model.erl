-module(util_model).

-include("../../include/auto/db.hrl").
-include("../../include/properties.hrl").

-export([get_player_id/0,
		find_count/1, 
		find_all/1,
		find/2,
		save_all/0]).

get_player_id() ->
	[{User, _}] = get(user),
	User#user.uuid.

create(Record) -> ok.
	
find_count(Term) ->
	case get(Term) of 
		undefined -> 0;
		[] -> 0;
		Records -> length(Records)
	end.
	
find_all(Term) ->
	case get(Term) of 
		undefined -> [];
		[] -> [];
		Records -> 
			lists:foldl(fun({Record, _}, AccIn) ->
				[Record|AccIn]
			end, [], Records)
	end.

find(Term, Key) ->
	case get(Term) of 
		undefined -> undefined;
		[] -> undefined;
		Records -> 
			case lists:keyfind(Key, 2, Records) of
				 false -> undefined;
				{Record, _} -> Record
			end
	end.

save_all() ->
	lists:foreach(fun(Table) ->
		case get(Table) of
			undefined -> ok; 
			[] -> ok;
			Records ->
				Res = lists:foldl(fun({Record, State}, AccIn) ->
					if 
						State =:= ?STATE_MODEL_CREATE ->
							Sql = sql_format:create(Record),
							erl_db:execute(Sql),
							[{Record, ?STATE_MODEL_LOAD}|AccIn];
						true ->
							Sql = sql_format:update(Record),
							erl_db:execute(Sql),
							[{Record, State}|AccIn]
					end
				end, [], Records),
				put(Table, Res)
		end
	end, ?DB_TABLE_NAMES). 
