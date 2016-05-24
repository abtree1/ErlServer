-module(alliance_sup).

-behaviour(supervisor).
%% API
-export([start_link/0, start_child/1]).
-export([init/1, get_pid/1, offline/1, stop/0]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Uuid) ->
    case lookup({alliance, Uuid}) of 
        false ->
            {ok, Pid} = supervisor:start_child(?MODULE, [Uuid]),
            insert({alliance, Uuid}, Pid),
            Pid;
        Pid -> Pid
    end.

get_pid(Uuid) -> start_child(Uuid).

offline(Uuid) ->
    delete({alliance, Uuid}).

stop() ->
    case all() of 
        [] -> ok;
        Alliances ->
            lists:foreach(fun({_Key, Pid})->
                alliance:stop(Pid)
            end, Alliances)
    end.

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

all() ->
    ets:tab2list(?MODULE).