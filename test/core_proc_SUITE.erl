-module(core_proc_SUITE).

%% ct callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% tests
-export([test_1/1]).
-export([test_2/1]).
-export([test_3/1]).
-export([test_4/1]).
-export([test_5/1]).
-export([test_6/1]).
-export([lookup_meta/1]).
-export([update_meta/1]).
-export([link_1/1]).

-include_lib("common_test/include/ct.hrl").

-define(F,
    fun() ->
        receive
            _ -> ok
        after
            1000 -> ok
        end
    end
).

all() ->
    [test_1, test_2, test_3, test_4, test_5, test_6, lookup_meta, update_meta, link_1].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(core),
    Config.

end_per_suite(Config) ->
    application:stop(core),
    Config.

test_1(_Config) ->
    Pid = spawn(?F),
    ok = core_proc:register(Pid, proc1),
    {error, already_registered} = core_proc:register(Pid, proc1),
    Pid = core_proc:lookup(proc1),
    ok = core_proc:unregister(proc1),
    ok = core_proc:unregister(proc2),
    undefined = core_proc:lookup(proc1).

test_2(_Config) ->
    Pid = spawn(?F),
    ok = core_proc:register(Pid, proc1),
    Pid ! test,
    timer:sleep(100),
    undefined = core_proc:lookup(proc1).

test_3(_Config) ->
    Pid = spawn(?F),
    ok = core_proc:register(Pid, proc1),
    timer:sleep(1100),
    undefined = core_proc:lookup(proc1).

test_4(_Config) ->
    Self = self(),
    ok = core_proc:register(proc1),
    Self = core_proc:lookup(proc1),
    [{proc1, Self, #{}, #{}}] = core_proc:all().

test_5(_Config) ->
    P = spawn(
        fun() ->
            timer:sleep(1000),
            core_proc:register(proc1),
            receive
                _ -> ok
            end
        end
    ),
    undefined = core_proc:await(proc1, 1000),
    P = core_proc:await(proc1, 1000),
    P ! stop,
    timer:sleep(100),
    undefined = core_proc:await(proc1, 1000).

test_6(_Config) ->
    undefined = core_proc:await(proc1, 1000),
    P = spawn(
        fun() ->
            core_proc:register(proc1),
            receive
                _ -> ok
            end
        end
    ),
    P = core_proc:await(proc1, 1000),
    P ! stop,
    timer:sleep(100),
    undefined = core_proc:await(proc1, 1000).

lookup_meta(_Config) ->
    P = spawn(?F),
    ok = core_proc:register(P, proc1, #{}, #{sid => hello}),
    {P, #{sid := hello}} = core_proc:lookup(proc1, meta),
    P ! stop,
    timer:sleep(20),
    undefined = core_proc:lookup(proc1, meta).

update_meta(_Config) ->
    P = spawn(?F),
    ok = core_proc:register(P, proc1),
    {P, #{}} = core_proc:lookup(proc1, meta),
    true = core_proc:update_meta(proc1, #{true => false}),
    {P, #{true := false}} = core_proc:lookup(proc1, meta),
    P ! stop,
    timer:sleep(100),
    undefined = core_proc:lookup(proc1).

link_1(_Config) ->
    P = spawn(?F),
    ok = core_proc:register(P, proc1, #{link => false}),
    P = core_proc:lookup(proc1),
    P ! stop,
    timer:sleep(20),
    P = core_proc:lookup(proc1),
    false = core_proc:unregister(proc1),
    undefined = core_proc:lookup(proc1).

