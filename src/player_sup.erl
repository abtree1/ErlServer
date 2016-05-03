-module(player_sup).
-behaviour(supervisor).
-include("../include/properties.hrl").
%% API
-export([start_link/0, start_child/1]).
-export([init/1, get_pid/1, offline/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child([Socket]) ->
    Uuid = uuid_factory:get_uuid(),
    {ok, Pid} = supervisor:start_child(?MODULE, [{Uuid, Socket}]),
    insert({player, Uuid}, {Socket, Pid}),
    Pid.

get_pid(Uuid) ->
    case lookup({player, Uuid}) of
            false -> false;
            {_, Pid} = Pid 
    end.

offline(Uuid) ->
    delete({player, Uuid}).

init([]) ->
    ets:new(?MODULE,[ordered_set, public, named_table, {keypos, 1}, {read_concurrency, true}]),
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(undefined, player, worker, [])]}}.

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