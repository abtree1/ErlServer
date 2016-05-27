-module(user_controller).

-export([login/2,
		change_name/2]).

login(_PlayerId, {}) ->  %% old player
	after_login(),
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
					after_login(),
					user_model:info()
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
after_login() -> ok.
