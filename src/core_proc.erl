-module(core_proc).

-compile({no_auto_import, [register/2]}).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/1]).
-export([register/2]).
-export([register/3]).
-export([register/4]).
-export([lookup/1]).
-export([lookup/2]).
-export([unregister/1]).
-export([all/0]).
-export([await/2]).
-export([update_meta/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-type opts() :: #{
    link => boolean()
}.
-export_type([opts/0]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{spawn_opt, [{priority, high}]}]).

-spec register(Name :: term()) -> ok | {error, already_registered}.
register(Name) ->
    register(self(), Name).

-spec register(Pid :: pid(), Name :: term()) -> ok | {error, already_registered}.
register(Pid, Name) ->
    register(Pid, Name, #{link => true}).

register(Pid, Name, Opts) ->
    register(Pid, Name, Opts, #{}).

register(Pid, Name, Opts, Meta) ->
    case ets:lookup(?MODULE, Pid) of
        [{Pid, _Name}] ->
            {error, already_registered};
        [] ->
            Link = maps:get(link, Opts, true),
            case ets:insert_new(?MODULE, {Name, Pid, Opts, Meta}) of
                true when Link =:= true ->
                    _ = ets:insert_new(?MODULE, {Pid, Name}),
                    gen_server:cast(?MODULE, {link, Pid});
                true when Link =:= false ->
                    _ = ets:insert_new(?MODULE, {Pid, Name}),
                    ok;
                false ->
                    {error, already_registered}
            end
    end.

-spec lookup(Name :: term()) -> undefined | pid().
lookup(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            undefined;
        [{Name, Pid, _Opts, _Meta}] ->
            Pid
    end.

-spec lookup(Name :: term(), meta) -> undefined | {pid(), term()}.
lookup(Name, meta) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            undefined;
        [{Name, Pid, _OPts, Meta}] ->
            {Pid, Meta}
    end.

-spec unregister(Name :: term()) -> ok | false.
unregister(Name) ->
    case ets:take(?MODULE, Name) of
        [{Name, Pid, #{link := Link}, _Meta}] ->
            true = ets:delete(?MODULE, Pid),
            Link andalso gen_server:cast(?MODULE, {unlink, Pid});
        [] -> ok
    end.

-spec all() -> [{Name :: term(), Pid :: pid()}].
all() ->
    Filter = fun({Name, _Pid, _Opts, _Meta} = E, Acc) when not is_pid(Name) -> [E | Acc];
                ({_Pid, _Name}, Acc) -> Acc
             end,
    ets:foldl(Filter, [], ?MODULE).

-spec await(Name :: term(), Timeout :: non_neg_integer()) -> undefined | pid().
await(Name, Timeout) ->
    await(Name, 3, Timeout div 3, Timeout rem 3).

await(_Name, 0, _Timeout, _Rem) ->
    undefined;
await(Name, Tries, Timeout, Rem) ->
    case lookup(Name) of
        undefined when Tries - 1 =:= 1 ->
            timer:sleep(Timeout),
            await(Name, Tries - 1, Timeout + Rem, Rem);
        undefined ->
            timer:sleep(Timeout),
            await(Name, Tries - 1, Timeout, Rem);
        Pid -> Pid
    end.

-spec update_meta(Name :: term(), Meta :: map()) -> boolean().
update_meta(Name, Meta) ->
    ets:update_element(?MODULE, Name, {4, Meta}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_cast({link, Pid}, State) ->
    true = link(Pid),
    {noreply, State};
handle_cast({unlink, Pid}, State) ->
    true = unlink(Pid),
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    case ets:take(?MODULE, Pid) of
        [] -> ok;
        [{Pid, Name}] ->
            true = ets:delete(?MODULE, Name)
    end,
    {noreply, State}.
