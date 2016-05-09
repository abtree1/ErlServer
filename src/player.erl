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
-export([account_enter/2,
        new_player/2,
        recv_msg/2,
        send_data/1,
        send_data/2,
        stop/1]).

-record(state, {player_id, socket}).

start_link({PlayerId, Socket}) ->
    gen_server:start_link(?MODULE, [PlayerId, Socket], []).

new_player(Pid, Data) ->
    gen_server:cast(Pid, {new_player, Data}).

account_enter(Pid, Term) -> 
    gen_server:cast(Pid, {account_enter, Term}).

recv_msg(Pid, Msg) ->
    gen_server:cast(Pid, {message, Msg}).

send_data(Data) ->
    Socket = get(socket),
    gen_tcp:send(Socket, Data).

send_data(PlayerId, Data) ->

    case player_sup:get_pid(PlayerId) of 
        false -> false;
        Pid -> gen_server:cast(Pid, {send, Data})
    end.

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerId, Socket]) ->
    error_logger:info_msg("player,init: ~p,~p~n", [PlayerId, Socket]),
    put(socket, Socket),
    {ok, #state{player_id=PlayerId, socket=Socket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({account_enter, Term}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [{account_enter, Term}]),
    {Term, Module, Fun} = lists:keyfind(Term, 1, ?PROTOCONTROLLER),
    erlang:apply(Module, Fun, [State#state.player_id, {}]),
    {noreply, State};
handle_cast({new_player, {Term, Account, Passwd}}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [{new_player, Account}]),
    {Term, Module, Fun} = lists:keyfind(Term, 1, ?PROTOCONTROLLER),
    erlang:apply(Module, Fun, [State#state.player_id, {Account, Passwd}]),
    {noreply, State};
handle_cast({message, {Term, Data}}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [{Term, Data}]),
    {Term, Module, Fun} = lists:keyfind(Term, 1, ?PROTOCONTROLLER),
    erlang:apply(Module, Fun, [State#state.player_id, Data]),
    {noreply, State};
handle_cast({send, Data}, State) ->
    gen_tcp:send(State#state.socket, Data),
    {stop, State};
handle_cast(stop, State) ->
    player_sup:offline(State#state.player_id),
    erase(socket),
    {stop, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg("Player dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State=#state{player_id=PlayerId}) ->
    error_logger:info_msg("Player: ~p, Terminate With Reason: ~p~n", [PlayerId, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% private api
%%%===================================================================