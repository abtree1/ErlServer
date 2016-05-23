-module(user_controller).

-export([login/2,
		change_name/2,
		create_alliance/2]).

login(_PlayerId, {}) ->  %% old player
	user_model:info();
login(PlayerId, {Account, Passwd}) -> %% new player
	user_model:create_player(PlayerId, {Account, Passwd}),
	{new_user_name}.

change_name(_PlayerId, {Name}) ->
	case erl_config:has_dirty_word(Name) of
		true -> {fail, {<<"error_username_dirty">>}};
		false ->
			case util_model:filter(user, name, Name) of
				fail -> {fail, {<<"error_username_used">>}};
				_ ->
					user_model:change_name(Name),
					user_model:info()
			end
	end.

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
							alliance_sup:start_child(AllianceId),
							alliance:async_proxy(AllianceId, alliance_model, create, [{AllianceId, AllianceName, PlayerId}]),
							user_model:create_alliance(AllianceId),
							{ok}
					end
			end
	end.
