-module(user_model).

-include("../../include/auto/db.hrl").
-include("../../include/properties.hrl").

-export([create_player/2,
		info/0,
		change_name/1,
		has_alliance/0,
		create_alliance/1]).
-export([get_alliance_id/0,
		get_player_id/0,
		load/1,
		lookup/0,
		update/1]).

create_player(PlayerId, {Accout, Passwd}) ->
	User = #user{uuid = PlayerId, name = <<"">>, level = 1,account = Accout, passwd=Passwd},
	util_model:create(user, User),
	util_model:save_all().

info() ->
	User = lookup(),
	{user, {User#user.uuid, 
			User#user.name, 
			User#user.level, 
			User#user.account, 
			User#user.alliance_id}}.

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

create_alliance(AllianceId) ->
	User = lookup(),
	NewUser = User#user{alliance_id = AllianceId},
	update(NewUser).

get_player_id() ->
	case get(user) of
		undefined -> undefined;
		[{Uuid, _User, _State}] -> Uuid
	end.

get_alliance_id() ->
	User = lookup(),
	User#user.alliance_id.

load(PlayerId) ->
	util_model:load(user, uuid, PlayerId).

lookup() ->
	util_model:find(user).

update(User) ->
	util_model:update(user, User).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	