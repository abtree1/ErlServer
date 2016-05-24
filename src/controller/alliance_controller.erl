-module(alliance_controller).

-export([create_alliance/2, info/2]).

create_alliance(PlayerId, {AllianceName}) ->
	case user_model:has_alliance() of 
		true -> {fail, {<<"error_create_has_alliance">>}};
		false ->
			case erl_config:has_dirty_word(AllianceName) of 
				true -> {fail, {<<"error_create_alliance_name_dirty">>}};
				false ->
					case util_model:filter(alliance, name, AllianceName) of
						fail -> {fail, {<<"error_create_alliance_name_used">>}};
						_ ->
							AllianceId = uuid_factory:get_uuid(),
							user_model:create_alliance(AllianceId),
							alliance:proxy(AllianceId, alliance_model, create, [{AllianceId, AllianceName, PlayerId}])
					end
			end
	end.

info(_PlayerId, _) ->
	case user_model:get_alliance_id() of 
		undefined -> {fail, {<<"error_user_out_alliance">>}};
		0 -> {fail, {<<"error_user_out_alliance">>}};
		AllianceId ->
			alliance:proxy(AllianceId, alliance_model, info, [])
	end.