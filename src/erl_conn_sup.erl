-module(erl_conn_sup).
-behaviour(supervisor).
-include("../include/properties.hrl").
%% API
-export([start_link/0, start_child/1]).
-export([init/1, conn_begin/0]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

conn_begin() ->
    {ok, Sock} = gen_tcp:listen(?TCP_LISTEN_PORT, [binary, {packet, 1}, {active, false}]),
    accept(Sock).

accept(LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
    {ok, Pid} = start_child([Sock]),
    error_logger:info_msg("erl_conn_sup:accept: ~p~n", [Pid]),
    erl_conn:do_recv(Pid),
    accept(LSock).

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(undefined, erl_conn, worker, [])]}}.
