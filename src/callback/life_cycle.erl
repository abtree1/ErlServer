-module(life_cycle).

-export([after_start/0]).

after_start() ->
	erl_config:init().