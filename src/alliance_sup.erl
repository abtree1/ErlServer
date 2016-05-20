-module(alliance_sup).

-behaviour(supervisor).
%% API
-export([start_link/0, start_child/1]).
-export([init/1, get_pid/1, offline/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Uuid) ->
    case get_pid(Uuid) of
        false ->
            {ok, Pid} = supervisor:start_child(?MODULE, [Uuid]),
            insert({alliance, Uuid}, Pid),
            Pid;
        Pid -> Pid 
    end.

get_pid(Uuid) ->
    case lookup({alliance, Uuid}) of
        false -> false;
       	Pid -> Pid 
    end.

offline(Uuid) ->
    delete({alliance, Uuid}).

init([]) ->
    ets:new(?MODULE,[ordered_set, public, named_table, {keypos, 1}, {read_concurrency, true}]),
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(undefined, alliance, worker, [])]}}.

%%%===================================================================
%%% private api
%%%===================================================================
insert(Key, Tuple) ->
    ets:insert(?MODULE, {Key, Tuple}).

lookup(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Tuple}] -> Tuple;
        [] -> false
    end.

delete(Key) ->
    ets:delete(?MODULE, Key).