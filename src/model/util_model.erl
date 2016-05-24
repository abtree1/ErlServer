-module(util_model).

-include("../../include/auto/db.hrl").
-include("../../include/properties.hrl").

-export([create/2,
		find_count/1, 
		find_all/1,
		find/1,
		find/2,
		filter/3,
		update/2,
		delete/2,
		load/3,
		save_all/0,
		save_all_sync/0]).

create(Table, Record) -> 
	sure_load_data(Table),
	Uuid = record_mapper:get_field(Record, uuid),
	%% update_load_models(Table),
	case get(Table) of 
		undefined -> 
			put(Table, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		[] -> 
			put(Table, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		Records ->
			case lists:keyfind(Uuid, 1, Records) of 
				false -> 
					put(Table, [{Uuid, Record, ?STATE_MODEL_CREATE}|Records]), 
					Record;
				_-> false
			end
	end.
	
find_count(Table) ->
	sure_load_data(Table),
	case get(Table) of 
		undefined -> 0;
		[] -> 0;
		Records -> length(Records)
	end.
	
find_all(Table) ->
	sure_load_data(Table),
	case get(Table) of 
		undefined -> [];
		[] -> [];
		Records -> 
			lists:foldl(fun({_Uuid, Record, _State}, AccIn) ->
				[Record|AccIn]
			end, [], Records)
	end.

filter(Table, Field, Value) ->
	Sql = sql_format:find_by(Table, Field, Value),
	Records = erl_db:load(Sql, Table, record_mapper:get_mapping(Table)),
	case Records of 
		[] -> ok;
		_ -> fail
	end.

find(Table) ->
	sure_load_data(Table),
	case get(Table) of 
		undefined -> undefined;
		[] -> undefined;
		Records ->
			[{_Key, Record, _State}|_] = Records,
			Record
	end.

find(Table, Key) ->
	sure_load_data(Table),
	case get(Table) of 
		undefined -> undefined;
		[] -> undefined;
		Records -> 
			case lists:keyfind(Key, 1, Records) of
				 false -> undefined;
				{Key, Record, _State} -> Record
			end
	end.

update(Table, Record) ->
	sure_load_data(Table),
	Uuid = record_mapper:get_field(Record, uuid),
	%% update_load_models(Table),
	case get(Table) of 
		undefined -> 
			put(Table, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		[] -> 
			put(Table, [{Uuid, Record, ?STATE_MODEL_CREATE}]),
			Record;
		Records ->
			case lists:keyfind(Uuid, 1, Records) of 
				false -> 
					put(Table, [{Uuid, Record, ?STATE_MODEL_CREATE}|Records]), 
					Record;
				_ -> 
					NewRecords = lists:keyreplace(Uuid, 1, Records, {Uuid, Record, ?STATE_MODEL_LOAD}),
					put(Table, NewRecords),
					Record
			end
	end.

delete(Table, Key) ->
	sure_load_data(Table),
	case get(Table) of 
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

load(Table, Field, Value) ->
	Rules = get_serialize_rule(Table),
	Sql = sql_format:find_by(Table, Field, Value),
	Records = erl_db:load(Sql, Table, record_mapper:get_mapping(Table)),
	Res = lists:foldl(fun(Record, AccIn) ->
			Uuid = record_mapper:get_field(Record, uuid),
			NewRecord = lists:foldl(fun(SerField, RecordIn) ->
				SerValue = record_mapper:get_field(RecordIn, SerField),
				CodeSerValue = base64:decode(SerValue),
				record_mapper:set_field(RecordIn, SerField, binary_to_term(CodeSerValue))
			end, Record, Rules),
			[{Uuid, NewRecord, ?STATE_MODEL_LOAD}|AccIn]
	end, [], Records),
	put(Table, Res),
	update_load_models(Table).

save_all() ->
	case get(load_models) of
		undefined -> fail;
		[] -> fail;
		List ->
			lists:foreach(fun(Table) ->
				execute_save(Table)
			end, List)
	end.

save_all_sync() ->
	case get(load_models) of
		undefined -> fail;
		[] -> fail;
		List ->
			lists:foreach(fun(Table) ->
				execute_save_sync(Table)
			end, List)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_load_models(Table) ->
	case get(load_models) of
		undefined -> put(load_models, [Table]);
		[] -> put(load_models, [Table]);
		List ->
			case lists:member(Table) of 
				false -> put(load_models, [Table|List]);
				true -> ok 
			end
	end.

execute_save(Table) ->
	case get(Table) of
		undefined -> ok; 
		[] -> ok;
		Records ->
			Res = lists:foldl(fun({Uuid, Record, State}, AccIn) ->
				if 
					State =:= ?STATE_MODEL_CREATE ->
						NewRecord = serialize(Table, Record),
						Sql = sql_format:create(NewRecord),
						erl_db:execute(Sql),
						[{Uuid, Record, ?STATE_MODEL_LOAD}|AccIn];
					State =:= ?STATE_MODEL_LOAD ->
						NewRecord = serialize(Table, Record),
						Sql = sql_format:update_by(uuid, Uuid, NewRecord),
						erl_db:execute(Sql),
						[{Uuid, Record, State}|AccIn];
					State =:= ?STATE_MODEL_DELETE ->
						Sql = sql_format:delete_by(Table, uuid, Uuid),
						erl_db:execute(Sql),
						AccIn
				end
			end, [], Records),
			put(Table, Res)
	end.

execute_save_sync(Table) ->
	case get(Table) of
		undefined -> ok; 
		[] -> ok;
		Records ->
			Res = lists:foldl(fun({Uuid, Record, State}, AccIn) ->
				if 
					State =:= ?STATE_MODEL_CREATE ->
						NewRecord = serialize(Table, Record),
						Sql = sql_format:create(NewRecord),
						erl_db:execute_sync(Sql),
						[{Uuid, Record, ?STATE_MODEL_LOAD}|AccIn];
					State =:= ?STATE_MODEL_LOAD ->
						NewRecord = serialize(Table, Record),
						Sql = sql_format:update_by(uuid, Uuid, NewRecord),
						erl_db:execute_sync(Sql),
						[{Uuid, Record, State}|AccIn];
					State =:= ?STATE_MODEL_DELETE ->
						Sql = sql_format:delete_by(Table, uuid, Uuid),
						erl_db:execute_sync(Sql),
						AccIn
				end
			end, [], Records),
			put(Table, Res)
	end.

sure_load_data(Table) ->
	case is_load_data(Table) of 
		false ->
			PlayerID = case get(state_record) of
				{_, Uuid, _} -> Uuid;
				{_, Uuid} -> Uuid
			end, 
			Module = list_to_atom(atom_to_list(Table) ++ "_model"),
			Module:load(PlayerID);
		true -> ok
	end.

is_load_data(Table) ->
	case get(load_models) of
		undefined -> false; 
		[] -> false;
		List -> lists:member(Table, List) 
	end.

get_serialize_rule(Table) ->
	Module = list_to_atom(atom_to_list(Table) ++ "_model"),
	case lists:keyfind(serialize, 1, Module:module_info(attributes)) of 
		false -> [];
		{serialize, Rule} -> Rule
	end.

serialize(Table, Record) ->
	Rules = get_serialize_rule(Table),
	lists:foldl(fun(SerField, AccIn) ->
		Value = record_mapper:get_field(AccIn, SerField),
		BinValue = term_to_binary(Value),
		record_mapper:set_field(AccIn, SerField, base64:encode(BinValue))
	end, Record, Rules).