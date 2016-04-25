-module(erl_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% 先生成配置文件
    lager:start(),
    %% erl_config_file:decompress(),
    ensure_started(erl_config),
    R = erl_server_sup:start_link(),
    life_cycle:after_start(),
    R.

stop(_State) ->
    ok.

-spec ensure_started(module()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
