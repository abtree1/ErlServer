-module(user_controller).

-export([login/2]).

login(PlayerId, {}) ->  %% old player
	user_model:load_player(PlayerId);
login(PlayerId, {Account, Passwd}) -> %% new player
	user_model:create_player(PlayerId, {Account, Passwd}).