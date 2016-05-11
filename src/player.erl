-module(player).
-behaviour(gen_server).
-include("../include/auto/proto_controller.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([account_enter/3,
        new_player/3,
        recv_msg/2,    
        stop/1]).
-export([send_data/1,
        send_data/2,
        proxy/4,
        async_proxy/4,
        wrap/2,
        async_wrap/2]).

-record(state, {player_id, socket}).

start_link({PlayerId, Socket}) ->
    gen_server:start_link(?MODULE, [PlayerId, Socket], []).

new_player(Pid, Data, Socket) ->
    gen_server:cast(Pid, {new_player, Data, Socket}).

account_enter(Pid, Term, Socket) -> 
    gen_server:cast(Pid, {account_enter, Term, Socket}).

recv_msg(Pid, Msg) ->
    gen_server:cast(Pid, {message, Msg}).

send_data(Data) ->
    Socket = get(socket),
    if 
        Socket =:= undefined -> ok;
        true -> gen_tcp:send(Socket, Data)
    end.

send_data(PlayerId, Data) ->
    case player_sup:get_pid(PlayerId) of 
        false -> false;
        Pid -> gen_server:cast(Pid, {send, Data})
    end.

proxy(PlayerId, Module, Fun, Args) -> 
    case ownthread(PlayerId) of 
        true -> apply(Module, Fun, Args);
        false -> gen_server:call(player_sup:get_pid(PlayerId), {proxy, Module, Fun, Args})
    end.
async_proxy(PlayerId, Module, Fun, Args) ->
    gen_server:cast(player_sup:get_pid(PlayerId), {proxy, Module, Fun, Args}).

wrap(PlayerId, Fun) ->
    case ownthread(PlayerId) of 
        true -> Fun();
        false -> gen_server:call(player_sup:get_pid(PlayerId), {wrap, Fun})
    end.

async_wrap(PlayerId, Fun) ->
    gen_server:cast(PlayerId, {wrap, Fun}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerId, Socket]) ->
    %% error_logger:info_msg("player,init: ~p,~p~n", [PlayerId, Socket]),
    put(socket, Socket),
    {ok, #state{player_id=PlayerId, socket=Socket}}.

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
handle_cast({account_enter, Term, Socket}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [{account_enter, Term, Socket}]),
    put(socket, Socket),
    {Term, Module, Fun} = lists:keyfind(Term, 1, ?PROTOCONTROLLER),
    erlang:apply(Module, Fun, [State#state.player_id, {}]),
    {noreply, State#state{socket = Socket}};
handle_cast({new_player, {Term, Account, Passwd}, Socket}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [{new_player, Account}]),
    put(socket, Socket),
    {Term, Module, Fun} = lists:keyfind(Term, 1, ?PROTOCONTROLLER),
    erlang:apply(Module, Fun, [State#state.player_id, {Account, Passwd}]),
    {noreply, State#state{socket = Socket}};
handle_cast({message, {Term, Data}}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [{Term, Data}]),
    {Term, Module, Fun} = lists:keyfind(Term, 1, ?PROTOCONTROLLER),
    erlang:apply(Module, Fun, [State#state.player_id, Data]),
    {noreply, State};
handle_cast({send, Data}, State) ->
    if 
        State#state.socket =:= undefined -> ok;
        true -> gen_tcp:send(State#state.socket, Data)
    end,
    {noreply, State};
handle_cast(stop, State) ->
    save_all(),
    player_sup:offline(State#state.player_id),
    erase(socket),
    {stop, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg("Player dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State=#state{player_id=PlayerId, socket=Socket}) ->
    gen_tcp:close(Socket),
    save_all(),
    error_logger:info_msg("Player: ~p, Terminate With Reason: ~p~n", [PlayerId, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% private api
%%%===================================================================
ownthread(PlayerId) ->
    Uuid = util_model:get_player_id(),
    if 
        Uuid =:= PlayerId -> true;
        true -> false
    end.

save_all() -> util_model:save_all().
