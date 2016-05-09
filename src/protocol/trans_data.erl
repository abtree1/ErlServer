-module(trans_data).

-include("../../include/auto/proto_rules.hrl").
-include("../../include/auto/proto_enum.hrl").

-export([encode/1, decode/1]).

encode({Term, Data}) ->
	{_, Rules} = lists:keyfind(Term, 1, ?PROTORULE),
    {_, Id} = lists:keyfind(Term, 1, ?PROTONAMEENUM),
    util_protocol:encode_tuple({Id, Data}, {short, Rules}).

decode(<<>>) -> false;
decode(Bin) ->
	Size = util_protocol:get_bit_size(short),
	Size1 = erlang:bit_size(Bin),
	if 
		Size > Size1 -> false;
		true ->
			{Id, BinLeft} = util_protocol:decode_short(Bin),
			case lists:keyfind(Id, 1, ?PROTOIDENUM) of 
				false -> false;
				{Id, Term} ->
		    		{_, Rules} = lists:keyfind(Term, 1, ?PROTORULE),
		    		{Data, _} = util_protocol:decode_tuple(BinLeft, Rules),
		    		{Term, Data}
		    end
	end.