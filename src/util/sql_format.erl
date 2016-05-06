-module(sql_format).

-export([account_enter/1]).

account_enter(Account) ->
	format("select uuid, account, passwd from user where account = '~s';", [Account]).

format(Formater, Values) ->
    String = io_lib:format(Formater, Values),
    list_to_binary(String).