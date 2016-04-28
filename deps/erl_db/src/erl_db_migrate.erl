-module(erl_db_migrate).

-include("../../include/properties.hrl").

-export([migrate/0]).

migrate() ->
	case file:read_file(?DATABASE_SQL) of 
		{error, _} -> error;
		{ok, Context} -> analyse_sql(Context)
	end.

analyse_sql(Context) ->
	Lists = binary_string:split(Context, <<"->">>),
	Olds = erl_counter:get({db, migrate}),
	Res = deal_items(Lists, Olds, []),
	erl_counter:set({db, migrate}, Res),
	create_records().

deal_items([], _Olds, Results) -> Results.
deal_items([Index, Sql|Lists], Olds, Results) ->
	case lists:member(Index) of 
		true -> deal_items(Lists, Olds, [Index|Results]);
		false ->
			execute_sql(Sql),
			deal_items(Lists, Olds, [Index|Results])
	end.

execute_sql(Sql) -> ok.

create_records() ->
	Tables = select(<<"show tables;">>);
	Records = lists:foldl(fun(Table, AccIn) ->
		Columns = select(<<"show columns from table ">>, [])
	end, [], Tables),
	file:write_file(?DATABASE_HRL, Records).
