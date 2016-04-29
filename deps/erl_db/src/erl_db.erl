-module(erl_db).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([migrate/0,
        execute/1,
        select/1,
        load/2,
        load/3]).

-include("../../include/properties.hrl").

-record(state, {}).

-define(DB_POOL, ?MODULE).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

migrate() ->
    erl_db_migrate:migrate(?DB_POOL).

%% execute sql without return
execute(Sql) -> 
    gen_server:cast(?SERVER, {execute, Sql}).

%% execute sql with return
select(Sql) -> 
    { _, _, _, Result, _ } = emysql:execute(?DB_POOL, Sql),
    Result.

%% execute sql return records
load(Sql, RecordName) ->
    Fields = record_mapper:get_mapping(RecordName),
    load(Sql, RecordName, Fields).
load(Sql, RecordName, Fields) ->
    Result = emysql:execute(?DB_POOL, Sql),
    emysql_util:as_record(Result, RecordName, Fields).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    init_db_pool(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({execute, Sql}, State) ->
    emysql:execute(?DB_POOL, Sql),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% private function
%%%===================================================================
init_db_pool() ->
    emysql:add_pool(?DB_POOL, [{size, ?DATABASE_S},
                     {user, ?DATABASE_U},
                     {password, ?DATABASE_P},
                     {host, ?DATABASE_H},
                     {port, ?DATABASE_O},
                     {database, ?DATABASE},
                     {encoding, ?DATABASE_C}]).