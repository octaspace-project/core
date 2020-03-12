-module(core).

-export([uptime/0]).

-spec uptime() -> non_neg_integer().
uptime() ->
    {T, _} = erlang:statistics(wall_clock),
    T div 1000.
