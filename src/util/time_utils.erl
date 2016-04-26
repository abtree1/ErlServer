-module(time_utils).

-export([now/0,
         one_week/0,
         current_time/0,
         get_now_daynum/0,
         current_month_days/0,
         remain_seconds_to_tomorrow/0,
         begin_of_today/0,
         end_of_today/0,
         datetime_to_timestamp/1,
         datetime_to_timestamp/2,
         current_time_to_now/1,
         time_to_seconds/3,
         datetime/0,
         to_i/1,
         date_number/0,
         number_date/1,
         time_string_to_timestamp/1,
         day_diff/2,
         timestamp_to_datetime/1,
         datetime_string/0]).

-define(ORI_SECONDS, 62167219200).

now() ->
    current_time().

one_week() ->
    604800.

current_time() ->
    {MegaSecs, Secs, _} = erlang:now(),
    MegaSecs * 1000000 + Secs.

% 获取今天星期数
get_now_daynum() ->
    {Date, _} = calendar:universal_time(),
    calendar:day_of_the_week(Date).

% 获取当前月的天数
current_month_days() ->
    {{Year, Month, _}, _} = calendar:universal_time(),
    Days = calendar:last_day_of_the_month(Year, Month),
    {Year, Month, Days}.

remain_seconds_to_tomorrow() ->
    end_of_today() - current_time().

end_of_today() ->
    calendar:datetime_to_gregorian_seconds({date(),{24,0,0}}) - ?ORI_SECONDS.

begin_of_today() ->
    calendar:datetime_to_gregorian_seconds({date(),{0,0,0}}) - ?ORI_SECONDS.

datetime_to_timestamp(Date, Time) ->
    calendar:datetime_to_gregorian_seconds({Date, Time}) - ?ORI_SECONDS.

datetime_to_timestamp(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?ORI_SECONDS.

current_time_to_now(CurrentTime) ->
    MegaSecs = CurrentTime div 1000000,
    Secs = CurrentTime rem 1000000,
    {MegaSecs, Secs, 0}.

time_to_seconds(MegaSecs, Secs, _MicroSecs) ->
    MegaSecs * 1000000 + Secs.

datetime() ->
    {datetime, {erlang:date(), erlang:time()}}.

to_i({datetime, {Date, Time}}) ->
    calendar:datetime_to_gregorian_seconds({Date, Time})  - ?ORI_SECONDS.

date_number() ->
    {Year, Month, Day} = date(),
    Year * 10000 + Month * 100 + Day.

number_date(Datenumber) ->
    Y = Datenumber div 10000,
    M = Datenumber rem 10000 div 100,
    D = Datenumber rem 100,
    {Y, M, D}.

time_string_to_timestamp(TimeString) ->
    [HourStr, MinutesStr] = binary_string:split(TimeString, <<":">>),
    Hour = erlang:binary_to_integer(HourStr),
    Minutes = erlang:binary_to_integer(MinutesStr),
    calendar:datetime_to_gregorian_seconds({date(),{Hour, Minutes,0}}) - ?ORI_SECONDS.

timestamp_to_datetime(TimeStamp) ->
    calendar:now_to_datetime(time_utils:current_time_to_now(TimeStamp)).

day_diff(T1, T2) ->
    N1 = current_time_to_now(T1),
    N2 = current_time_to_now(T2),
    {DT1, _} = calendar:now_to_datetime(N1),
    {DT2, _} = calendar:now_to_datetime(N2),
    calendar:date_to_gregorian_days(DT2) - calendar:date_to_gregorian_days(DT1).

datetime_string() ->
    {Year, Month, Day} = erlang:date(),
    {H, M, S} = erlang:time(),
    list_to_binary(io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", 
                                 [Year, Month, Day, H, M, S])).
