all:
	./rebar get-deps
	./rebar compile

console:
	erl -pa ebin deps/*/ebin\
		-config erl_server.config \
		-s erl_server start

clean:
	./rebar clean