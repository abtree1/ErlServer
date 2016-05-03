-module(player).
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
-export([recv_msg/2,
        send_data/2,
        stop/1]).

-record(state, {player_id, socket}).

start_link({PlayerId, Socket}) ->
    gen_server:start_link(?MODULE, [PlayerId, Socket], []).

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

handle_cast({message, Msg}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [Msg]),
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