-module(util_protocol).

-define (BOOLEAN,  8).
-define (CHAR,     8).
-define (SHORT,   16).
-define (INTEGER, 32).
-define (FLOAT,   32).
-define (STRING,  16).  %% string length 
-define (ARRAY,   16).  %% array loop

-export ([encode/1,
          decode/2,
          encode_bool/1,
          decode_bool/1,
          encode_char/1,
          decode_char/1,
          encode_short/1,
          decode_short/1,
          encode_integer/1,
          decode_integer/1,
          encode_float/1,
          decode_float/1,
          encode_array/2,
          decode_array/2,
          encode_tuple/1,
          encode_tuple/2,
          decode_tuple/2,
          encode_list/1,
          encode_list/2,
          decode_list/2,
          encode_string/1,
          decode_string/1]).
-export([get_byte_size/1,
        get_bit_size/1]).

get_byte_size(Term) ->
    case Term of
        bool -> ?BOOLEAN div 8;
        char -> ?CHAR div 8;
        short -> ?SHORT div 8;
        integer -> ?INTEGER div 8;
        float -> ?FLOAT div 8;
        string -> ?STRING div 8;
        array -> ?ARRAY div 8;
        _ -> undefined
    end.

get_bit_size(Term) ->
    case Term of
        bool -> ?BOOLEAN;
        char -> ?CHAR;
        short -> ?SHORT;
        integer -> ?INTEGER;
        float -> ?FLOAT;
        string -> ?STRING;
        array -> ?ARRAY;
        _ -> undefined
    end.

encode_bool(Bool) ->
	if 
		Bool =:= true -> <<1:?BOOLEAN>>;
		Bool =:= 1 -> <<1:?BOOLEAN>>;
		true -> <<0:?BOOLEAN>>
	end.

decode_bool(<<Bin/binary>>) ->
	<<Bool:?BOOLEAN, Data/binary>> = Bin,
	if
		Bool =:= 1 -> {true, Data};
		true -> {false, Data}
	end.

encode_char(Char) when Char =:= undefined -> <<0:?CHAR>>;
encode_char(Char) when Char =:= true -> <<1:?CHAR>>;
encode_char(Char) when Char =:= false -> <<0:?CHAR>>;
encode_char(Char) -> <<Char:?CHAR>>.

decode_char(<<Bin/binary>>) ->
	<<Char:?CHAR, Data/binary>> = Bin,
	{Char, Data}.

encode_short(Short) when Short =:= undefined -> <<0:?SHORT>>;
encode_short(Short) when is_integer(Short) -> <<Short:?SHORT>>;
encode_short(Short) when is_float(Short) ->
    Sh = trunc(Short),
    <<Sh:?SHORT>>.

decode_short(<<Bin/binary>>) ->
	<<Short:?SHORT/signed, Data/binary>> = Bin,
    {Short, Data}.

encode_integer(Integer) when Integer =:= undefined -> <<0:?INTEGER>>;
encode_integer(Integer) when is_integer(Integer) -> <<Integer:?INTEGER>>;
encode_integer(Integer) when is_float(Integer) ->
    Int = trunc(Integer),
    <<Int:?INTEGER>>.

decode_integer(<<Bin/binary>>) ->
	<<Integer:?INTEGER/signed, Data/binary>> = Bin,
    {Integer, Data}.

encode_float(Float) when Float =:= undefined -> <<0.0:?FLOAT/float>>;
encode_float(Float) -> <<Float:?FLOAT/float>>.

decode_float(<<Bin/binary>>) ->
	<<Float:?FLOAT/float, Data/binary>> = Bin,
	{Float, Data}.

encode_string(undefined) ->
    String = <<"">>,
    Length = byte_size(String),
    <<Length:?STRING/integer, String:Length/binary>>;
encode_string(String) when is_binary(String) ->
    Length = byte_size(String),
    <<Length:?STRING/integer, String:Length/binary>>;
encode_string(List) when is_list(List) ->
    String = list_to_binary(List),
    Length = byte_size(String),
    <<Length:?STRING/integer, String:Length/binary>>;
encode_string(Atom) when is_atom(Atom) ->
    String = atom_to_binary(Atom, utf8),
    Length = byte_size(String),
    <<Length:?STRING/integer, String:Length/binary>>.

decode_string(<<Bin/binary>>) ->
	<<Length:?STRING/integer, Data/binary>> = Bin,
	<<String:Length/binary, Data1/binary>> = Data,
	{String, Data1}.

encode_array(Array, Fun) when is_list(Array) ->
    Len = length(Array),
    DataList = [Fun(Item) || Item <- Array],
    list_to_binary([<<Len:?ARRAY/integer>>, DataList]).

decode_array(<<Bin/binary>>, Fun) ->
	<<ArrayLen:?ARRAY, Data/binary>> = Bin,
    decode_array(ArrayLen, Data, Fun, []).

decode_array(0, DataLeft, _Fun, Result) ->
    {lists:reverse(Result), DataLeft};
decode_array(ArrayLen, Data, Fun, Result) ->
    {Item, Bin} = Fun(Data),
    decode_array(ArrayLen - 1, Bin, Fun, [Item|Result]).

encode_tuple(Tuple) when is_tuple(Tuple) ->
    DataList = [encode(Item) || Item <- tuple_to_list(Tuple)],
    list_to_binary(DataList).

%%编码元祖
%%例子: {1, 1.0, <<"hello">>, ...}  编码规则: {integer, float, string, ...}
%%例子: {1, 1.0, <<"hello">>, [{1, 1.0, <<"world">>, ...}, ...]}  编码规则: {integer, float, string, [{integer, float, string, ...}], ...}
encode_tuple(Tuple, EncodeRule) when is_tuple(Tuple) ->
    Items = tuple_to_list(Tuple),
    Rules = tuple_to_list(EncodeRule),
    DataList = encode_tuple(Items, Rules, []),
    Bin = list_to_binary(DataList),
    Bin.

encode_tuple([], _, Res) -> lists:reverse(Res);
encode_tuple([Item|Items], [Rule|Rules], Res) ->
    Bin = case Rule of
        bool -> encode_bool(Item);
        integer -> encode_integer(Item);
        char -> encode_char(Item);
        short -> encode_short(Item);
        float -> encode_float(Item);
        string -> encode_string(Item);
        _ when is_tuple(Rule) -> encode_tuple(Item, Rule);
        _ when is_list(Rule) -> encode_list(Item, Rule)
    end,
    encode_tuple(Items, Rules, [Bin|Res]).

%% 解码元组
%% 例子: {1, 1.0, <<"hello">>, ...}  解码规则: {integer, float, string, ...}
%% 例子: {1, 1.0, <<"hello">>, [{1, 1.0, <<"world">>, ...}, ...]}  解码规则: {integer, float, string, [{integer, float, string, ...}], ...}
decode_tuple(<<Data/binary>>, DecodeRule) ->
    Rules = tuple_to_list(DecodeRule),
    {DecodedList, DataLeft} = decode_tuple(Data, Rules, []),
    {list_to_tuple(DecodedList), DataLeft}.

decode_tuple(Data, [], Res) -> {lists:reverse(Res), Data};
decode_tuple(Data, [Rule|Rules], Res) ->
    {Res1, DataLeft} = case Rule of 
        bool -> decode_bool(Data);
        integer -> decode_integer(Data);
        char -> decode_char(Data);
        short -> decode_short(Data);
        float -> decode_float(Data);
        string -> decode_string(Data);
        _ when is_tuple(Rule) -> decode_tuple(Data, Rule);
        _ when is_list(Rule) -> decode_list(Data, Rule)
    end,
    decode_tuple(DataLeft, Rules, [Res1|Res]).

encode_list(List) when is_list(List) ->
    Len = length(List),
    DataList = [encode(Item) || Item <- List],
    list_to_binary([<<Len:?ARRAY/integer>>, DataList]).

encode_list(List, [Rule]) when is_list(List) ->
    Len = length(List),
    DataList = case Rule of 
        bool -> [encode_bool(Item) || Item <- List];
        integer -> [encode_integer(Item) || Item <- List];
        char -> [encode_char(Item) || Item <- List];
        short -> [encode_short(Item) || Item <- List];
        float -> [encode_float(Item) || Item <- List];
        string -> [encode_string(Item) || Item <- List];
        _ when is_tuple(Rule) -> [encode_tuple(Item, Rule) || Item <- List];
        _ when is_list(Rule) -> [encode_list(Item, Rule) || Item <- List]
    end,
    list_to_binary([<<Len:?ARRAY/integer>>, DataList]).

%% 解码列表
%% 规则: 只能解码规则数组，即数组内每个元素的类型和结构必须一样
%% =====================基础解码例子==========================
%% 例子: [1, 2, 3, ...]  解码规则: [integer]
%% 例子: [1.0, 2.0, 3.0, ...]  解码规则: [float]
%% 例子: [<<"a">>, <<"b">>, <<"c">>, ...]  解码规则: [string]
%% 例子: [{1, 2.0, <<"hello">>, ...}, {2, 3.0, <<"world">>, ...}, ...]  解码规则: [{integer, float, string, ...}]
%% =====================嵌套解码例子==========================
%% 例子: [[1, 2, 3, ...], ...]  解码规则: [[integer]]
%% 例子: [[1.0, 2.0, 3.0, ...], ...]  解码规则: [[float]]
%% 例子: [[<<"a">>, <<"b">>, <<"c">>, ...], ...]  解码规则: [[string]]
%% 例子: [[{1, 2.0, <<"hello">>, ...}, {2, 3.0, <<"world">>, ...}, ...], ...]  解码规则: [[{integer, float, string, ...}]]
%% 例子: ...
decode_list(<<ListLen:?ARRAY, Data/binary>>, [RepeatElement | _]) ->
    TempLists = lists:seq(1, ListLen),
    case RepeatElement of 
        bool -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_bool(AccIn),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL};
        integer -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_integer(AccIn),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL};
        char -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_char(AccIn),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL};
        short -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_short(AccIn),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL};
        float -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_float(AccIn),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL};
        string -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_string(AccIn),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL};
        _ when is_tuple(RepeatElement) -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_tuple(AccIn, RepeatElement),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL};
        _ when is_list(RepeatElement) -> 
            {DataL, ResL} = lists:foldl(fun(_Temp, {AccIn, AccRes}) ->
                {Item, DataLe} = decode_list(AccIn, RepeatElement),
                {DataLe, [Item|AccRes]}
            end, {Data, []}, TempLists),
            {lists:reverse(ResL), DataL}
    end.

encode(Info) when is_integer(Info) ->
    encode_integer(Info);
encode(Info) when is_float(Info) ->
    encode_integer(Info);
encode(Info) when is_atom(Info) ->
    encode_string(Info);
encode(Info) when is_binary(Info) ->
    encode_string(Info);
encode(Info) when is_tuple(Info) ->
    encode_tuple(Info);
encode(Info) when is_list(Info) ->
    encode_list(Info).

%%数据解码
decode(<<Data/binary>>, DecodeRuleList) when is_tuple(DecodeRuleList) ->
  DecodedList = decode(Data, tuple_to_list(DecodeRuleList)),
  list_to_tuple(lists:delete(<<>>, DecodedList));
decode(<<>>, []) ->
    [<<>>];
decode(<<Data/binary>>, []) ->
    [Data];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when DecodeRule == integer ->
    {Integer, DataLeft} = decode_integer(Data),
    [Integer | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when DecodeRule == float ->
    {Float, DataLeft} = decode_float(Data),
    [Float | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when DecodeRule == string ->
    {String, DataLeft} = decode_string(Data),
    [String | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when is_tuple(DecodeRule) ->
    {Array, DataLeft} = decode_tuple(Data, DecodeRule),
    [Array | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when is_list(DecodeRule) ->
    {Array, DataLeft} = decode_list(Data, DecodeRule),
    [Array | decode(DataLeft, DecodeRuleList)].