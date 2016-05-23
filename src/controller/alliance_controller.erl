-module(alliance_controller).

-export([info/2]).

info(_PlayerId, _) ->
	case users_model:get_alliance_id() of 
		undefined -> {fail, <<"error_user_out_alliance">>};
		0 -> {fail, <<"error_user_out_alliance">>};
		AllianceId ->
			alliance:proxy(AllianceId, alliance_model, info, [])
	end.