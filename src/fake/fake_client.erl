-module(fake_client).

-include("../../include/auto/proto_rules.hrl").
-include("../../include/auto/proto_enum.hrl").

-export([connect/0,
		send/2,
		stop/1]).

connect() ->
	SomeHostInNet = "127.0.0.1", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5555, 
                                 [binary, {packet, 1}, {active, false}]),
    Sock.

send(Sock, {Term, Data}) ->
	{_, Rules} = lists:keyfind(Term, 1, ?PROTORULE),
	{_, Id} = lists:keyfind(Term, 1, ?PROTONAMEENUM),
	Bin = util_protocol:encode_tuple({Id, Data}, {short, Rules}),
	gen_tcp:send(Sock, Bin).
    %% {ok, Recv} = gen_tcp:recv(Sock, 0),
    %% Recv.

stop(Sock) ->
	gen_tcp:close(Sock).