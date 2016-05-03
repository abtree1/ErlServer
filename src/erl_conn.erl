-module(erl_conn).
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
-export([do_recv/1]).

-record(state, {socket}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

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
on_recv(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, B} when Pid =:= undefined->
            error_logger:info_msg("erl_conn:tcp, on_recv: ~p~n", [B]),
            Id = player_sup:start_child([Socket]),
            on_recv(Socket, Id);
        {ok, B} ->
            error_logger:info_msg("erl_conn:tcp, on_recv: ~p~n", [B]),
            player:recv_msg(Pid, B),
            on_recv(Socket, Pid);
        {error, closed} ->
            error_logger:info_msg("erl_conn:tcp, on_recv: stop"),
            player:stop(Pid),
            {ok, closed}
    end.