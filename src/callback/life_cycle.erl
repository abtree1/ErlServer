-module(life_cycle).

-export([config/0, 
		migrate/0, 
		protocol/0, 
		after_start/0]).

config() ->
	erl_config_file:decompress(),
	erl_config_dirtywords:decompress(),
	init:stop().

migrate() ->
	application:start(erl_server),
	erl_db:migrate(),
	init:stop().

protocol() ->
	parse_file:parse_protocol(),
	parse_file:parse_controller(),
	init:stop().

after_start() -> 
	erl_counter:start(),
	erl_conn_sup:conn_begin(),
	random:seed(os:timestamp()).