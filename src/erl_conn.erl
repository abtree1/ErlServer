-module(erl_conn).
-behaviour(gen_server).

-include("../include/properties.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([conn_begin/0, 
         do_recv/1]).

-record(state, {socket}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

conn_begin() ->
    {ok, Pid} = erl_conn_sup:start_child([undefined]),
    conn_begin(Pid).

conn_begin(Pid) ->
    gen_server:cast(Pid, {tcp, do_listen}).

do_recv(Pid) ->
    gen_server:cast(Pid, {tcp, do_recv}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket]) ->
    {ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({tcp, do_listen}, State) ->
    {ok, Sock} = gen_tcp:listen(?TCP_LISTEN_PORT, [binary, {packet, 1}, {active, false}]),
    on_accept(Sock),
    gen_tcp:close(State#state.socket),
    {stop, State#state{socket=Sock}};
handle_cast({tcp, do_recv}, State) ->
    on_recv(State#state.socket, undefined),
    gen_tcp:close(State#state.socket),
    {stop, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg("Player dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State=#state{socket=Socket}) ->
    error_logger:info_msg("Player: ~p, Terminate With Reason: ~p~n", [Socket, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% private api
%%%===================================================================
on_accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Pid} = erl_conn_sup:start_child([Sock]),
    error_logger:info_msg("erl_conn_sup:accept: ~p~n", [Pid]),
    erl_conn:do_recv(Pid),
    on_accept(LSock).

on_recv(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            Data = trans_data:decode(Bin),
            error_logger:info_msg("erl_conn:tcp, on_recv: ~p~n", [Data]),
            send_data(Socket),
            on_recv(Socket, Pid);
        % {ok, B} when Pid =:= undefined->
        %     error_logger:info_msg("erl_conn:tcp, on_recv: ~p~n", [B]),
        %     Id = player_sup:start_child([Socket]),
        %     on_recv(Socket, Id);
        % {ok, B} ->
        %     error_logger:info_msg("erl_conn:tcp, on_recv: ~p~n", [B]),
        %     player:recv_msg(Pid, B),
        %     on_recv(Socket, Pid);
        {error, closed} ->
            error_logger:info_msg("erl_conn:tcp, on_recv: stop"),
            player:stop(Pid),
            {ok, closed}
    end.

send_data(Socket) ->
    Bin = trans_data:encode({user, {<<"abc">>, <<"def">>, 32}}),
    gen_tcp:send(Socket, Bin).