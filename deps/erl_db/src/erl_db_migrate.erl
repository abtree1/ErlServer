-module(erl_db_migrate).

-include("../../include/properties.hrl").

-export([migrate/1]).

migrate(DBPool) ->
	put({migrate, dbpool}, DBPool),
	case file:read_file(?DATABASE_SQL) of 
		{error, _} -> error;
		{ok, Context} -> analyse_sql(Context)
	end,
	erase({migrate, dbpool}).

analyse_sql(Context) ->
	Lists = binary_string:split(Context, <<"->">>),
	Olds = case erl_counter:get({db, migrate}) of 
		undefined -> [];
		L -> L 
	end,
	%% error_logger:info_msg("analyse_sql: ~p~n", [Olds]),
	Res = deal_items(Lists, Olds, []),
	erl_counter:set({db, migrate}, Res),
	create_records().

deal_items([], _Olds, Results) -> Results;
deal_items([Index, Sql|Lists], Olds, Results) ->
	case lists:member(Index, Olds) of 
		true -> deal_items(Lists, Olds, [Index|Results]);
		false ->
			Pool = get({migrate, dbpool}),
 			emysql:execute(Pool, Sql),
			deal_items(Lists, Olds, [Index|Results])
	end.

create_records() ->
	Tables = select(<<"show tables;">>),
	{Records, Ns} = lists:foldl(fun([Table], {AccIn, Names}) ->
		List = io_lib:format("show columns from `~s`;", [Table]),
		Columns = select(list_to_binary(List)),
		Cols = lists:foldl(fun([Column|_pros], AccIn1) ->
			[Column|AccIn1]
		end, [], Columns),
		Cs = binary_string:join(lists:reverse(Cols), <<",">>),
		Record = << <<"-record(">>/binary, Table/binary, <<", {">>/binary, Cs/binary, <<"}).\r\n">>/binary >>,
		{[Record|AccIn], [Table|Names]}
	end, {[], []}, Tables),
	Nas = binary_string:join(Ns, <<",">>),
	Na = << <<"-define(DB_TABLE_NAMES, [">>/binary, Nas/binary, <<"]).\r\n">>/binary >>,
	file:write_file(?DATABASE_HRL, [Na|Records]).

select(Sql) ->
	Pool = get({migrate, dbpool}),
	{ _, _, _, Result, _ } = emysql:execute(Pool, Sql),
	Result.
