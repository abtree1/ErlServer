-module(alliance_tests).

-include_lib("eunit/include/eunit.hrl").

alliance_test() ->
	erl_server:start(),
	Sock = fake_client:connect(),
	fake_client:send(Sock, {request_account_enter, {<<"bbb">>, <<"111111">>}}),
	Res = fake_client:send(Sock, {request_create_alliance, {<<"aaas">>}}),
	%% Res1 = fake_client:send(Sock, {request_alliance_info}),
	{_, {_, Name, _, _, _, _, _, _}} = Res,
	erl_server:stop(),
	[?_assertEqual(Name, <<"aaas">>)].