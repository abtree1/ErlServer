-module(life_cycle).

-export([config/0, after_start/0]).

config() ->
	erl_config_file:decompress(),
	erl_config_dirtywords:decompress(),
	init:stop().

after_start() -> 
	erl_counter:start().