-module(alliance).
-behaviour(gen_server).
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([proxy/4,
        async_proxy/4,
        wrap/2,
        async_wrap/2]).
-record(state, {uuid}).

start_link(Uuid) ->
    gen_server:start_link(?MODULE, [Uuid], []).

proxy(AllianceId, Module, Fun, Args) -> 
    case ownthread(AllianceId) of 
        true -> apply(Module, Fun, Args);
        false -> gen_server:call(alliance_sup:get_pid(AllianceId), {proxy, Module, Fun, Args})
    end.
async_proxy(AllianceId, Module, Fun, Args) ->
    gen_server:cast(alliance_sup:get_pid(AllianceId), {proxy, Module, Fun, Args}).

wrap(AllianceId, Fun) ->
    case ownthread(AllianceId) of 
        true -> Fun();
        false -> gen_server:call(alliance_sup:get_pid(AllianceId), {wrap, Fun})
    end.

async_wrap(AllianceId, Fun) ->
    gen_server:cast(AllianceId, {wrap, Fun}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Uuid]) ->
    {ok, #state{uuid = Uuid}}.

handle_call({wrap, Fun}, _From, State) ->
    Result = Fun(),
    {reply, Result, State};
handle_call({proxy, Module, Fun, Args}, _From, State) ->
    Result = erlang:apply(Module, Fun, Args),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({wrap, Fun}, State) ->
    Fun(),
    {noreply, State};
handle_cast({proxy, Module, Fun, Args}, State) ->
    erlang:apply(Module, Fun, Args),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg("Alliance dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% private api
%%%===================================================================
ownthread(AllianceId) ->
    Uuid = alliance_model:get_alliance_id(),
    if 
        Uuid =:= AllianceId -> true;
        true -> false
    end.
