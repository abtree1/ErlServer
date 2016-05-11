-module(user_model).

-include("../../include/auto/db.hrl").
-include("../../include/properties.hrl").

-export([create_player/2,
		load_player/1]).

create_player(PlayerId, {Accout, Passwd}) ->
	User = #user{uuid = PlayerId, name = <<"">>, level = 1,account = Accout, passwd=Passwd},
	%% Sql = sql_format:create(User),
	%% erl_db:execute(Sql),
	put(user, [{User, ?STATE_MODEL_CREATE}]).

load_player(PlayerId) ->
	lists:foreach(fun(Table) ->
		Sql = sql_format:find_by(Table, uuid, PlayerId),
		Records = erl_db:load(Sql, Table, record_mapper:get_mapping(Table)),
		%% error_logger:info_msg("XXXXXXXXXX:~p~n", [Records]),
		Res = lists:foldl(fun(Record, AccIn) ->
			[{Record, ?STATE_MODEL_LOAD}|AccIn]
		end, [], Records),
		put(Table, Res)
	end, ?DB_TABLE_NAMES). 