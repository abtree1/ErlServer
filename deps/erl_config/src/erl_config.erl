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

-export([init/0, find/2]).

-record(state, {}).
-include("../../include/config.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init()->
    gen_server:cast(?SERVER, init).

find(TableName, Key) ->
    gen_server:call(?SERVER, {find, TableName, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({find, TableName, Key}, _From, State) ->
    Ret = case erl_config_store:lookup(TableName) of 
        false -> false;
        List ->
            case lists:keyfind(Key, 1, List) of
                false -> false;
                Tuple ->
                    List1 = tuple_to_list(Tuple),
                    list_to_tuple([TableName|List1])
            end
    end,
    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(init, State) ->
    erl_config_store:init(),
    error_logger:info_msg("init file"),
    erl_config_file:decompress(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
