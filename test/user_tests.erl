-module(user_tests).

-include_lib("eunit/include/eunit.hrl").

user_test() ->
	erl_server:start(),
	Sock = fake_client:connect(),
	Res1 = fake_client:send(Sock, {request_account_enter, {<<"aaa">>, <<"111111">>}}),
	{Name1, Account1} = case Res1 of 
		{user, {_, Name, _, Account, _}} -> {Name, Account};
		{new_user_name, {}} ->
			Res = fake_client:send(Sock, {request_insert_name, {<<"aaa">>}}),
			{user, {_, Name, _, Account, _}} = Res,
			{Name, Account}
	end,
	erl_server:stop(),
	[?_assertEqual(Name1, <<"aaa">>),
	?_assertEqual(Account1, <<"aaa">>)].