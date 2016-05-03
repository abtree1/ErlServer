-module(erl_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ErlServerSpec = ?CHILD(erl_server, erl_server, worker, []),
	ErlConnSupSpec = ?CHILD(erl_conn_sup, erl_conn_sup, supervisor, []),
	PlayerSupSpec = ?CHILD(player_sup, player_sup, supervisor, []),
    Specs = [ErlServerSpec, ErlConnSupSpec, PlayerSupSpec],
    {ok, { {one_for_one, 10, 10}, Specs} }.

