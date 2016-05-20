-module(user_model).

-include("../../include/auto/db.hrl").
-include("../../include/properties.hrl").

-export([create_player/2,
		load_player/1,
		change_name/1,
		has_alliance/0]).
-export([get_player_id/0,
		load/1,
		lookup/0,
		update/1]).

create_player(PlayerId, {Accout, Passwd}) ->
	User = #user{uuid = PlayerId, name = <<"">>, level = 1,account = Accout, passwd=Passwd},
	util_model:create(user, User),
	util_model:save_all().

load_player(PlayerId) ->
	load(PlayerId).

change_name(Name) ->
	User = lookup(),
	NewUser = User#user{name = Name},
	update(NewUser).

has_alliance() ->
	User = lookup(),
	case User#user.alliance_id of 
		undefined -> false;
		<<"">> -> false;
		_ -> true
	end.

get_player_id() ->
	case get(user) of
		undefined -> undefined;
		[{Uuid, _User, _State}] -> Uuid
	end.

load(PlayerId) ->
	util_model:load(user, uuid, PlayerId).

lookup() ->
	util_model:find(user).

update(User) ->
	util_model:find(user, User).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	