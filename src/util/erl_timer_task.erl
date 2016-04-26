-module(erl_timer_task).

-export([add_self/3,
		lookup_self/1,
		del_self/1]).
% -export([add/3,
% 		lookup/1,
% 		del_self/1,
% 		callback_self/1]).


% %%//////////////////////////////////////////////////
% %% timertask 任务挂在自己线程，需要自己线程handle info接收，性能更好，但必须回掉after_timer_task_self
% %%//////////////////////////////////////////////////
add_self(Time, Key, Msg) ->
	TimeRef = erlang:send_after(Time * 1000, self(), Msg),
	Now = time_utils:now(),
	erl_counter:set_timeout({timertask, Key}, Now + Time, TimeRef).

lookup_self(Key) ->
	case erl_counter:get_timeout({timertask, Key}) of
		undefined -> undefined;
		TimeRef -> 
			Time = erlang:read_timer(TimeRef),
			Time div 1000
	end.

del_self(Key) ->
	case erl_counter:get_timeout({timertask, Key}) of
		undefined -> undefined;
		TimeRef ->
			erl_counter:del_timeout({timertask, Key}),
			erlang:cancel_timer(TimeRef)
	end.

% %%//////////////////////////////////////////////////

% %%//////////////////////////////////////////////////
% %%挂在单独线程中
% %%//////////////////////////////////////////////////
% add(Time, Key, MFA) -> ok.

% update(Time, Key, MFA) -> ok.

% lookup(Key) -> ok.

% cancel(Key) -> ok.