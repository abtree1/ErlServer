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
        stop/1]).

-record(state, {player_id, socket}).

start_link({PlayerId, Socket}) ->
    gen_server:start_link(?MODULE, [PlayerId, Socket], []).

recv_msg(Pid, Msg) ->
    gen_server:cast(Pid, {message, Msg}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerId, Socket]) ->
    error_logger:info_msg("player,init: ~p,~p~n", [PlayerId, Socket]),
    {ok, #state{player_id=PlayerId, socket=Socket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({message, Msg}, State) ->
    error_logger:info_msg("player,handle_cast: ~p~n", [Msg]),
    gen_tcp:send(State#state.socket, <<"Return Data!">>),
    {noreply, State};
handle_cast(stop, State) ->
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