all:
	./rebar get-deps
	./rebar compile

console:
	erl -pa ebin deps/*/ebin\
		-config erl_server.config \
		-s erl_server start

config: 
	erl -pa ebin deps/*/ebin\
		-config erl_server.config \
		-s erl_config_file decompress

clean:
	./rebar clean