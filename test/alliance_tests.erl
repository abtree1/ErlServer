-module(alliance_tests).

-include_lib("eunit/include/eunit.hrl").

user_test_() ->
	erl_server:start(),
	Sock = fake_client:connect(),
	fake_client:send(Sock, {request_account_enter, {<<"aaa">>, <<"111111">>}}),
	fake_client:send(Sock, {request_create_alliance, {<<"aaas">>}}),
	Res = fake_client:send(Sock, {request_alliance_info}),
	{_, {_, Name, _, _, _}} = Res,
	erl_server:stop(),
	[?_assertEqual(Name, <<"aaas">>)].