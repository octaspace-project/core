-module(core_cast).

%% API
-export([to_string/1]).
-export([to_ip/1]).
-export([to_binary/1]).
-export([to_int/1]).
-export([to_atom/1]).
-export([int_fmt/1]).

-spec to_string(Data :: integer() | binary() | atom() | string()) -> string().
to_string(Data) when is_integer(Data) ->
    integer_to_list(Data);
to_string(Data) when is_binary(Data) ->
    binary_to_list(Data);
to_string(Data) when is_atom(Data) ->
    atom_to_list(Data);
to_string(Data) when is_list(Data) ->
    Data.

-spec to_ip(Data :: string() | binary() | inet:ip_address()) -> inet:ip_address().
to_ip(Data) when is_list(Data) ->
    {ok, IP} = inet:ip(Data), IP;
to_ip(Data) when is_binary(Data) ->
    to_ip(binary_to_list(Data));
to_ip({A, B, C, D} = IP) when
    A >= 0, A =< 255,
    B >= 0, B =< 255,
    C >= 0, C =< 255,
    D >= 0, D =< 255 ->
    IP.

-spec to_binary(Data :: string() | atom() | integer() | float() | binary()) -> binary().
to_binary(Data) when is_list(Data) ->
    list_to_binary(Data);
to_binary(Data) when is_atom(Data) ->
	atom_to_binary(Data, latin1);
to_binary(Data) when is_integer(Data) ->
    integer_to_binary(Data);
to_binary(Data) when is_float(Data) ->
    float_to_binary(Data);
to_binary(Data) when is_binary(Data) ->
	Data.

-spec to_int(Data :: binary() | string() | integer()) -> integer().
to_int(Data) when is_binary(Data) ->
    binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
	list_to_integer(Data);
to_int(Data) when is_integer(Data) ->
    Data.

-spec to_atom(Data :: binary() | string()) -> atom().
to_atom(Data) when is_binary(Data) ->
    binary_to_existing_atom(Data, latin1);
to_atom(Data) when is_integer(hd(Data)) ->
    try
        list_to_existing_atom(Data)
    catch
        _:_ ->
            list_to_atom(Data)
    end;
to_atom(Data) when is_atom(Data) ->
    Data.

-spec int_fmt(non_neg_integer()) -> binary().
int_fmt(N) when N < 10 ->
    <<$0, (integer_to_binary(N))/binary>>;
int_fmt(N) ->
    integer_to_binary(N).
