-module(erl_battle).
-behaviour(gen_server).

-include("../../include/properties.hrl").
%% API
-export([start_link/0]).
-export([prepare_battle/2, verify_battle/2]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

prepare_battle(PlayerId, Formation) ->
	% [{Pos, RoleId, Att, Def, HP, Speed},{Pos, RoleId, Att, Def, HP, Speed}...]
	Rates = get_random_list(?BATTLE_RANDOM_SIZE),
	% erl_counter:set({battle_verify, PlayerId}, {, Rates}),
	{ok, Rates}.
	%% gen_server:call(?SERVER, {prepare_battle, PlayerId, Formation}).

verify_battle(PlayerId, BattleList) ->
	{Attrs, Rates} = erl_counter:get({battle_verify, PlayerId}),
	verify(Attrs, BattleList, Rates).
	%% gen_server:call(?SERVER, {verify_battle, PlayerId, BattleList}).
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

%%%===================================================================
%%% internal function
%%%===================================================================
get_random_list(0, AccOut) -> AccIn;
get_random_list(N, AccIn) ->
	[random:uniform(?BATTLE_RANDOM_RATE)|AccIn].

verify(Attrs, BattleList, Rates) -> {success, 0}.