-module(user_model).

-include("../../include/auto/db.hrl").

-export([create_player/2,
		load_player/1]).

create_player(PlayerId, {Accout, Passwd}) ->
	User = #user{uuid = PlayerId, name = <<"">>, level = 1,account = Accout, passwd=Passwd},
	Sql = sql_format:create_model(User),
	erl_db:execute(Sql),
	put(user, User).

load_player(PlayerId) ->
	lists:foreach(fun(Table) ->
		Sql = sql_format:find_by(Table, uuid, PlayerId),
		Record = erl_db:load(Sql, Table, record_mapper:get_mapping(Table)),
		error_logger:info_msg("XXXXXXXXXX:~p~n", [Record]),
		put(Table, Record)
	end, ?DB_TABLE_NAMES).  
