-module(erl_global).
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
-record(state, {}).

-export([migrate/0,
        protocol/0,
        config/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%
%%%
migrate()->
    % io:fwrite(<<"xxxxxxxxxxxxxxxxxbegin ~n">>),
    application:start(crypto),
    application:start(erl_global),
    application:start(emysql),
    application:start(erl_counter),
    application:start(erl_db),
    erl_counter:start(),
    % io:fwrite(<<"xxxxxxxxxxxxxxxxxstart ~n">>),
    erl_counter:del({db, migrate}),
    erl_db:migrate(),
    init:stop().

protocol()->
    application:start(erl_global),
    parse_file:parse_protocol(),
    parse_file:parse_controller(),
    init:stop().

config()->
    application:start(erl_global),
    erl_config_file:decompress(),
    erl_config_dirtywords:decompress(),
    init:stop().

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    