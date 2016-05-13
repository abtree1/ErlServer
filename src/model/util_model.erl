-module(util_model).

-include("../../include/auto/db.hrl").
-include("../../include/properties.hrl").

-export([get_player_id/0,
		create/2,
		find_count/1, 
		find_all/1,
		find/2,
		update/2,
		delete/2,
		load_all/1,
		save_all/0]).

get_player_id() ->
	[{Uuid, _User, _State}] = get(user),
	Uuid.

create(Term, Record) -> 
	Uuid = record_mapper:get_field(Record, uuid),
	case get(Term) of 
		undefined -> 
			put(Term, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		[] -> 
			put(Term, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		Records ->
			case lists:keyfind(Uuid, 1, Records) of 
				false -> 
					put(Term, [{Uuid, Record, ?STATE_MODEL_CREATE}|Records]), 
					Record;
				_-> false
			end
	end.
	
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
			lists:foldl(fun({_Uuid, Record, _State}, AccIn) ->
				[Record|AccIn]
			end, [], Records)
	end.

find(Term, Key) ->
	case get(Term) of 
		undefined -> undefined;
		[] -> undefined;
		Records -> 
			case lists:keyfind(Key, 1, Records) of
				 false -> undefined;
				{Key, Record, _State} -> Record
			end
	end.

update(Term, Record) ->
	Uuid = record_mapper:get_field(Record, uuid),
	case get(Term) of 
		undefined -> 
			put(Term, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		[] -> 
			put(Term, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		Records ->
			case lists:keyfind(Uuid, 1, Records) of 
				false -> 
					put(Term, [{Uuid, Record, ?STATE_MODEL_CREATE}|Records]), 
					Record;
				_ -> 
					lists:keyreplace(Uuid, 1, Records, {Uuid, Record, ?STATE_MODEL_LOAD}),
					Record
			end
	end.

delete(Term, Key) ->
	case get(Term) of 
		undefined -> undefined;
		[] -> undefined;
		Records -> 
			case lists:keyfind(Key, 1, Records) of
				false -> undefined;
				{Key, _Record, ?STATE_MODEL_CREATE} -> 
					lists:keydelete(Key, 1, Records),
					ok;
				{Key, Record, ?STATE_MODEL_LOAD} -> 
					lists:keyreplace(Key, 1, Records, {Key, Record, ?STATE_MODEL_DELETE}),
					ok;
				_ -> ok
			end
	end.

load_all(PlayerId) ->
	lists:foreach(fun(Table) ->
		Field = case Table of 
			user -> uuid;
			_ -> user_id
		end,

  		Sql = sql_format:find_by(Table, Field, PlayerId),
		Records = erl_db:load(Sql, Table, record_mapper:get_mapping(Table)),
		%% error_logger:info_msg("XXXXXXXXXX:~p~n", [Records]),
		Res = lists:foldl(fun(Record, AccIn) ->
			Uuid = record_mapper:get_field(Record, uuid),
			[{Uuid, Record, ?STATE_MODEL_LOAD}|AccIn]
		end, [], Records),
		put(Table, Res)
	end, ?DB_TABLE_NAMES). 

save_all() ->
	lists:foreach(fun(Table) ->
		case get(Table) of
			undefined -> ok; 
			[] -> ok;
			Records ->
				Res = lists:foldl(fun({Uuid, Record, State}, AccIn) ->
					if 
						State =:= ?STATE_MODEL_CREATE ->
							Sql = sql_format:create(Record),
							erl_db:execute(Sql),
							[{Uuid, Record, ?STATE_MODEL_LOAD}|AccIn];
						State =:= ?STATE_MODEL_LOAD ->
							Sql = sql_format:update_by(uuid, Uuid, Record),
							erl_db:execute(Sql),
							[{Uuid, Record, State}|AccIn];
						State =:= ?STATE_MODEL_DELETE ->
							Sql = sql_format:delete_by(Table, uuid, Uuid),
							erl_db:execute(Sql),
							AccIn
					end
				end, [], Records),
				put(Table, Res)
		end
	end, ?DB_TABLE_NAMES). 
