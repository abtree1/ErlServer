-module(trans_data).

-include("../../include/auto/proto_rules.hrl").
-include("../../include/auto/proto_enum.hrl").

-export([encode/1, decode/1]).

encode({Term, Data}) ->
	{_, Rules} = lists:keyfind(Term, 1, ?PROTORULE),
    {_, Id} = lists:keyfind(Term, 1, ?PROTONAMEENUM),
    util_protocol:encode_tuple({Id, Data}, {short, Rules}).

decode(Bin) ->
	{Id, BinLeft} = util_protocol:decode_short(Bin),
	{Id, Term} = lists:keyfind(Id, 1, ?PROTOIDENUM), 
    {_, Rules} = lists:keyfind(Term, 1, ?PROTORULE),
    Data = util_protocol:decode_tuple(BinLeft, Rules),
    {Term, Data}.