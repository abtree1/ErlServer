-module(user_model).

-include("../../include/auto/db.hrl").
-include("../../include/properties.hrl").

-export([create_player/2,
		load_player/1]).

create_player(PlayerId, {Accout, Passwd}) ->
	User = #user{uuid = PlayerId, name = <<"">>, level = 1,account = Accout, passwd=Passwd},
	util_model:create(user, User).

load_player(PlayerId) ->
	util_model:load_all(PlayerId).
	