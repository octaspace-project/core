-module(core_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(TABLES, [
    {core_proc, [
        {read_concurrency, true},
        {write_concurrency, true}
    ]}
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Specs = [
        #{id => core_proc, start => {core_proc, start_link, []}}
    ],
    ok = create_tables(),
    {ok, {{one_for_one, 10, 5}, Specs}}.

create_tables() ->
    lists:foreach(fun create_table/1, ?TABLES).

create_table({Table, Options}) ->
    Table = ets:new(Table, [named_table, public | Options]).
