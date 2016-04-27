-module(erl_timer_task).

-export([add_self/3,
		lookup_self/1,
		del_self/1]).
-export([add/4,
		add/5,
		update/4,
		update/5,
		lookup/1,
		delete/1]).


% %%//////////////////////////////////////////////////
% %% timertask 任务挂在自己线程，需要自己线程handle info接收，性能更好, 但停服后不可恢复
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
%% 可恢复型 timertask
add(Time, Key, M, F, A) ->
	delete(Key),
	Now = time_utils:now(),
	{ok, Tref} = timer:apply_after(timer:seconds(Time), M, F, A),
	T = Now + Time,
	erl_counter:set_timeout({timertask, Key}, T, {T, Tref, M, F, A}).

%% 不可恢复型 timertask
add(Time, Key, Pid, Msg) ->
	delete(Key), 
	Now = time_utils:now(),
	{ok, Tref} = timer:send_after(timer:seconds(Time), Pid, Msg),
	T = Now + Time,
	erl_counter:set_timeout({timertask, Key}, T, {T, Tref}).

update(Time, Key, M, F, A) -> 
	add(Time, Key, M, F, A).

update(Time, Key, Pid, Msg) -> 
	add(Time, Key, Pid, Msg).

lookup(Key) ->
	case erl_counter:get_timeout({timertask, Key}) of 
		undefined -> 0;
		{Time, _Ref} ->
			Now = time_utils:now(),
			Time - Now
	end.
	
delete(Key) -> 
	case erl_counter:get_timeout({timertask, Key}) of 
		undefined -> ok;
		{_, Ref} -> timer:cancel(Ref)
	end.