-module(binary_string).

-export([split/2, join/2]).

split(BinaryString, Separator) ->
	split(BinaryString, Separator, []).

split(BinaryString, Separator, Result) ->
    case binary:split(BinaryString, Separator) of
        [BinaryString] ->
            lists:reverse([BinaryString|Result]);
        [Head, RemainBinaryString] ->
            split(RemainBinaryString, Separator, [Head|Result])
    end.

join(BinaryStringList, Separator) ->
    join(BinaryStringList, Separator, <<>>).

join([], _Separator, Result) ->
    Result;
join([BinaryString|BinaryStringList], Separator, <<>>) ->
    join(BinaryStringList, Separator, BinaryString);
join([BinaryString|BinaryStringList], Separator, Result) ->
    join(BinaryStringList, Separator, <<Result/binary, Separator/binary, BinaryString/binary>>).
