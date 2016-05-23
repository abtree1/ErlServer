-module(player_sup).
-behaviour(supervisor).
-include("../include/properties.hrl").
%% API
-export([start_link/0, start_child/1, start_child/2]).
-export([init/1, get_pid/1, offline/1, stop/0]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Socket) ->
    Uuid = uuid_factory:get_uuid(),
    start_child(Socket, Uuid).

start_child(Socket, Uuid) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [{Uuid, Socket}]),
    insert({player, Uuid}, {Socket, Pid}),
    Pid.

get_pid(Uuid) ->
    case lookup({player, Uuid}) of
        false -> false;
        {_, Pid} -> Pid 
    end.

offline(Uuid) ->
    delete({player, Uuid}).

stop() ->
    case all() of 
        [] -> ok;
        Players ->
            lists:foreach(fun({_Key, {_Socket, Pid}})->
                player:stop(Pid)
            end, Players)
    end.

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

all() ->
    ets:tab2list(?MODULE).