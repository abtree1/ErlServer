-module(erl_config).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([find/2,
        all/1,
        has_dirty_word/1]).

-record(state, {}).
-include("../../include/auto/config.hrl").
-include("../../include/auto/config_data.hrl").
-include("../../include/auto/dirtywords.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% init()->
%     gen_server:cast(?SERVER, init).

find(TableName, Key) ->
    gen_server:call(?SERVER, {find, TableName, Key}).

all(TableName) ->
    gen_server:call(?SERVER, {all, TableName}).

has_dirty_word(Str) ->
    NewStr = case is_binary(Str) of 
        true -> binary_to_list(Str);
        false -> Str
    end,
    gen_server:call(?SERVER, {dirty_words_filter, NewStr}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({find, TableName, Key}, _From, State) ->
    %% for ets branch
    %% Ret = case erl_config_store:lookup(TableName) of 
    Ret = case lists:keyfind(TableName, 1, ?CONFIGMAP) of 
        false -> false;
        {TableName, List} ->
            case lists:keyfind(Key, 2, List) of
                false -> false;
                Tuple -> Tuple
                    % List1 = tuple_to_list(Tuple),
                    % list_to_tuple([TableName|List1])
            end
    end,
    {reply, Ret, State};
handle_call({all, TableName}, _From, State) ->
    %% for ets branch
    %% Ret = case erl_config_store:lookup(TableName) of 
    Ret = case lists:keyfind(TableName, 1, ?CONFIGMAP) of 
        false -> [];
        {TableName, List} -> List
    end,
    {reply, Ret, State};
handle_call({dirty_words_filter, Str}, _From, State) ->
    Ret = match_dirtywords(Str, ?DIRTYWORDS),
    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% handle_cast(init, State) ->
%     %% for ets branch
%     %% erl_config_store:init(),
%     %% error_logger:info_msg("init file"),
%     erl_config_file:decompress(),
%     {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% private function
%%%===================================================================
match_dirtywords([], _List) -> false;
match_dirtywords(Datas, List) ->
    case match_dirtywords_pre(Datas, List) of 
        fail -> 
            [_|L] = Datas,
            match_dirtywords(L, List);
        ok -> true
    end.

match_dirtywords_pre(_, stop) -> ok;
match_dirtywords_pre([], _) -> fail;
match_dirtywords_pre([Data|Datas], List) ->
    case lists:keyfind(<<Data>>,1,List) of 
        false -> fail;
        {_, L} -> match_dirtywords_pre(Datas, L)
    end.