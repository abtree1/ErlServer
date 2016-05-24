-module(alliance_model).

-include("../../include/auto/db.hrl").

-serialize([officers, elites, members]).

-export([create/1,
		info/0]).
-export([get_alliance_id/0,
		load/1,
		lookup/0,
		update/1]).

create({AllianceId, AllianceName, PlayerId}) -> 
	Alliance = #alliance{uuid = AllianceId, 
						name = AllianceName,
						level = 1,
						coins = 0,
						president = PlayerId,
						officers = [],
						elites = [],
						members = [PlayerId]},
	util_model:create(alliance, Alliance),
	util_model:save_all(),
	info().

info() ->
	Alliance = lookup(),
	{alliance, {Alliance#alliance.uuid,
			   Alliance#alliance.name,
			   Alliance#alliance.level,
			   Alliance#alliance.coins,
			   Alliance#alliance.president,
			   Alliance#alliance.officers,
			   Alliance#alliance.elites,
			   Alliance#alliance.members}}.

get_alliance_id() ->
	case get(alliance) of
		undefined -> undefined; 
		[{Uuid, _Alliance, _State}] -> Uuid
	end.

load(AllianceId) ->
	util_model:load(alliance, uuid, AllianceId).

lookup() ->
	util_model:find(alliance).

update(Alliance) ->
	util_model:find(alliance, Alliance).