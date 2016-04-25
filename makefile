.PHONY: all config start console clean

all:
	./rebar get-deps
	./rebar compile

console:
	erl -pa ebin deps/*/ebin\
		-config erl_server.config \
		-s erl_server start

start: config
	erl -pa ebin deps/*/ebin\
		-config erl_server.config \
		-s erl_server start

config: 
	erl -pa ebin deps/*/ebin\
		-config erl_server.config \
		-s life_cycle config
	./rebar compile

clean:
	./rebar clean