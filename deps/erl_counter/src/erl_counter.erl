-module(erl_counter).

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

-export([start/0,
	incr_daily_action/1,
	incr_daily_action/2,
	get_daily_action/1,
	del_daily_action/1, 
	get/1,
	set/2,
	del/1,
	get_timeout/1,
	set_timeout/3,
	del_timeout/1,
	get_incr/1,
	incr/1,
	incr/2,
	del_incr/1,
	daily_clear/0]).

-record(state, {}).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() -> 
	gen_server:cast(?SERVER, init).

incr_daily_action(Name) ->
	incr_daily_action(Name, 1).

incr_daily_action(Name, Count) -> 
	gen_server:cast(?SERVER, {incr_daily_action, Name, Count}).

get_daily_action(Name) -> 
	gen_server:call(?SERVER, {get_daily_action, Name}).

del_daily_action(Name) -> 
	gen_server:cast(?SERVER, {del_daily_action, Name}).

get(Name) -> 
	gen_server:call(?SERVER, {get, Name}).

set(Name, Value) -> 
	gen_server:cast(?SERVER, {set, Name, Value}).

del(Name) -> 
	gen_server:cast(?SERVER, {del, Name}).

get_timeout(Name) -> 
	gen_server:call(?SERVER, {get_timeout, Name}).

set_timeout(Name, Time, Value) -> 
	gen_server:cast(?SERVER, {set_timeout, Name, Time, Value}).

del_timeout(Name) -> 
	gen_server:cast(?SERVER, {del_timeout, Name}).

get_incr(Name) ->
	gen_server:call(?SERVER, {get_incr, Name}).

incr(Name) ->
	incr(Name, 1).

incr(Name, Amount) ->
	gen_server:cast(?SERVER, {incr, Name, Amount}).

del_incr(Name) ->
	gen_server:cast(?SERVER, {del_incr, Name}).

daily_clear() ->
	gen_server:cast(?SERVER, daily_clear).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({get_daily_action, Name}, _From, State) ->
	Count = erl_counter_mnesia:get_daily_counter(Name),
    {reply, Count, State};
handle_call({get, Name}, _From, State) ->
	Value = erl_counter_mnesia:get(Name),
    {reply, Value, State};
handle_call({get_incr, Name}, _From, State) ->
	Count = erl_counter_mnesia:get_incr(Name),
    {reply, Count, State};
handle_call({get_timeout, Name}, _From, State) ->
	Value = erl_counter_mnesia:get_timeout(Name),
    {reply, Value, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(init, State) ->
	erl_counter_mnesia:start(),
    {noreply, State};
handle_cast({incr_daily_action, Name, Count}, State) ->
	erl_counter_mnesia:incr_daily_counter(Name, Count),
    {noreply, State};
handle_cast({del_daily_action, Name}, State) ->
	erl_counter_mnesia:del_daily_counter(Name),
    {noreply, State};
handle_cast({set, Name, Value}, State) ->
	erl_counter_mnesia:set(Name, Value),
    {noreply, State};
handle_cast({del, Name}, State) ->
	erl_counter_mnesia:del(Name),
    {noreply, State};
handle_cast({set_timeout, Name, Time, Value}, State) ->
	erl_counter_mnesia:set_timeout(Name, Time, Value),
    {noreply, State};
handle_cast({del_timeout, Name}, State) ->
	erl_counter_mnesia:del_timeout(Name),
    {noreply, State};
handle_cast({incr, Name, Amount}, State) ->
	erl_counter_mnesia:incr(Name, Amount),
    {noreply, State};
handle_cast({del_incr, Name}, State) ->
	erl_counter_mnesia:del_incr(Name),
    {noreply, State};
handle_cast(daily_clear, State) ->
	erl_counter_mnesia:clean_daily_counters(),
	erl_counter_mnesia:clear_timeout_counter(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.