-module(core_datetime).

-export([rfc1123/0]).
-export([weekday/1]).
-export([month/1]).
-export([datetime_to_timestamp/1]).

%% Thu, 9 Aug 2018 19:16:40 GMT
rfc1123() ->
    {{Year, Month, Day}, {HH, MM, SS}} = calendar:universal_time(),
    <<
        (weekday(calendar:day_of_the_week(Year, Month, Day)))/binary,
        ", ",
        (integer_to_binary(Day))/binary,
        " ",
        (month(Month))/binary,
        " ",
        (integer_to_binary(Year))/binary,
        " ",
        (core_cast:int_fmt(HH))/binary,
        ":",
        (core_cast:int_fmt(MM))/binary,
        ":",
        (core_cast:int_fmt(SS))/binary,
        " GMT"
    >>.

-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
month(1)  -> <<"Jan">>;
month(2)  -> <<"Feb">>;
month(3)  -> <<"Mar">>;
month(4)  -> <<"Apr">>;
month(5)  -> <<"May">>;
month(6)  -> <<"Jun">>;
month(7)  -> <<"Jul">>;
month(8)  -> <<"Aug">>;
month(9)  -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

%% {{2020,6,5},{19,37,13}} -> 1591385844
-spec datetime_to_timestamp(DateTime :: calendar:datetime()) -> pos_integer().
datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
