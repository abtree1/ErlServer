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
        async_wrap/2,
        stop/1]).

-define(SAVE_TIME, 600). %10 minutes
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
    gen_server:cast(alliance_sup:get_pid(AllianceId), {wrap, Fun}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Uuid]) ->
    erl_timer_task:add_self(?SAVE_TIME, self(), save_all),
    State = #state{uuid = Uuid},
    put(state_record, State),
    {ok, State}.

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
handle_cast(stop, State) ->
    util_model:save_all(),
    {stop, {shutdown, data_persisted}, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(save_all, State) ->
    util_model:save_all(),
    erl_timer_task:add_self(?SAVE_TIME, self(), save_all),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:info_msg("Alliance dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    if 
        Reason =:= {shutdown, data_persisted} -> ok;
        true -> util_model:save_all()
    end,
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
