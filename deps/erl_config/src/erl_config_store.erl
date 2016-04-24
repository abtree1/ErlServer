-module(erl_config_store).
-export([init/0,insert/2,lookup/1,delete/1]).
-define(TABLE_ID,?MODULE).

init() ->
	ets:new(?TABLE_ID,[ordered_set, protected, named_table, {keypos, 1}, {read_concurrency, true}]),
	ok.

insert(Key, List) ->
	ets:insert(?TABLE_ID, {Key, List}).

lookup(Key) ->
	case ets:lookup(?TABLE_ID, Key) of
		[{Key, List}] -> List;
		[] -> false
	end.

delete(Key) ->
	ets:delete(?TABLE_ID, Key).