-module(erl_timer_task).

% -export([add_self/2,
% 		lookup_self/1,
% 		del_self/1,
% 		callback_self/1]).
% -export([add/3,
% 		lookup/1,
% 		del_self/1,
% 		callback_self/1]).


% %%//////////////////////////////////////////////////
% %% timertask 任务挂在自己线程，需要自己线程handle info接收，性能更好，但必须回掉after_timer_task_self
% %%//////////////////////////////////////////////////
% add_self(Time, Key, Msg) ->
% 	TimeRef = send_after(Time * 1000, self(), Msg),
% 	erl_counter:set({timertask, Key}, TimeRef).

% lookup_self(Key) ->
% 	case erl_counter:get({timertask, Key}) of
% 		undefined -> undefined;
% 		TimeRef -> 
% 			Time = read_timer(TimeRef),
% 			Time div 1000
% 	end.

% del_self(Key) ->
% 	case erl_counter:get({timertask, Key}) of
% 		undefined -> undefined;
% 		TimeRef ->
% 			erl_counter:del({timertask, Key}),
% 			cancel_timer(TimeRef)
% 	end.

% callback_self(Key)->
% 	erl_counter:del({timertask, Key}).
% %%//////////////////////////////////////////////////

% %%//////////////////////////////////////////////////
% %%挂在单独线程中
% %%//////////////////////////////////////////////////
% add(Time, Key, MFA) -> ok.

% update(Time, Key, MFA) -> ok.

% lookup(Key) -> ok.

% cancel(Key) -> ok.