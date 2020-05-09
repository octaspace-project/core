-module(core).

-export([uptime/0]).
-export([env/1]).
-export([env/2]).

-spec uptime() -> non_neg_integer().
uptime() ->
    {T, _} = erlang:statistics(wall_clock),
    T div 1000.

env(Option) ->
    env(Option, undefined).

env(Option, Default) ->
    application:get_env(application:get_env(core, app, core), Option, Default).
