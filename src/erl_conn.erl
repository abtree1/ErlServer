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
    {stop, shutdown, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
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
    erl_conn:do_recv(Pid),
    on_accept(LSock).

on_recv(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            case trans_data:decode(Bin) of 
                false -> {ok, closed};
                {Term, Data} ->
                    error_logger:info_msg("erl_conn:tcp, on_recv: ~p, ~p~n", [Term, Data]),
                    NPid = case Term of 
                        request_account_enter -> account_enter(Socket, {Term, Data}, Pid);
                        _ when Pid =:= undefined -> undefined;
                        _ -> 
                            player:recv_msg(Pid, {Term, Data}),
                            Pid
                    end,
                    on_recv(Socket, NPid)
            end;
        {error, closed} ->
            error_logger:info_msg("erl_conn:tcp, on_recv: stop"),
            player:stop(Pid),
            {ok, closed}
    end.

account_enter(Socket, {Term, {Account, Passwd}}, Pid) ->
    Sql = sql_format:account_enter(Account),
    MD5Pwd = md5:md5(Passwd),
    Res = erl_db:select(Sql),
    case Res of 
        [[_Uuid, _Account, LoadPasswd]] when MD5Pwd =/= LoadPasswd ->
            send_data(Socket, {fail, {<<"error_login_passwd">>}}),
            Pid;
        [[Uuid, _Account, _LoadPasswd]] when Pid =:= undefined -> 
            Nid = case player_sup:get_pid(Uuid) of 
                false -> player_sup:start_child(Socket, Uuid);
                Id -> Id
            end,
            player:account_enter(Nid, Term, Socket),
            Nid;
        [[_Uuid, _Account, _LoadPasswd]] ->
            player:account_enter(Pid, Term, Socket),
            Pid;
        _ when Pid =:= undefined -> 
            Id = player_sup:start_child(Socket),
            player:new_player(Id, {Term, Account, MD5Pwd}, Socket),
            Id;
        _ -> 
            player:new_player(Pid, {Term, Account, MD5Pwd}, Socket),
            Pid
    end.

send_data(Socket, Data) ->
    Bin = trans_data:encode(Data),
    gen_tcp:send(Socket, Bin).