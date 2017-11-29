.PHONY: all config start console clean db proto map test

all:
	./rebar get-deps
	./rebar compile

console:
	erl -pa ebin deps/*/ebin \
		-config erl_server.config \
		-s erl_server start

start: config db proto map
	erl -pa ebin deps/*/ebin \
		-config erl_server.config \
		-s erl_server start

config: 
	./rebar get-deps
	erl -pa ebin deps/*/ebin \
		-config erl_server.config \
		-s erl_global config
	#./rebar compile

db: 
	erl -pa ebin deps/*/ebin \
		-config erl_server.config \
		-s erl_global migrate
	#./rebar compile

proto: 
	./rebar get-deps
	erl -pa ebin deps/*/ebin \
		-config erl_server.config \
		-s erl_global protocol
	#./rebar compile

map:
	erl -pa ebin deps/*/ebin \
		-config erl_server.config \
		-s life_cycle map
	./rebar compile

test:
	./rebar compile eunit skip_deps=true

clean:
	./rebar clean