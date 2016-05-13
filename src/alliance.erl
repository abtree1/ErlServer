-module(alliance).
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
-record(state, {uuid}).

start_link(Uuid) ->
    gen_server:start_link(?MODULE, [Uuid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Uuid]) ->
    %% error_logger:info_msg("player,init: ~p,~p~n", [PlayerId, Socket]),
    {ok, #state{uuid = Uuid}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg("Player dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.