-module(erl_conn_sup).
-behaviour(supervisor).
%% API
-export([start_link/0, start_child/1]).
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(undefined, erl_conn, worker, [])]}}.