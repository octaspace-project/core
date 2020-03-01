-module(core_bstr).

-export([join/1]).
-export([join/2]).
-export([prefix/2]).

-spec join(List :: [binary()]) -> binary().
join(List) when is_list(List) ->
    << <<E/binary>> || E <- List >>.

-spec join(List :: [binary()], Sep :: binary()) -> binary().
join([], _Sep) -> <<>>;
join([Part], _Sep) -> Part;
join(List, Sep) ->
    F = fun(A, B) ->
        if byte_size(B) > 0->
            <<A/binary, Sep/binary, B/binary>>;
           true -> A
        end
    end,
    lists:foldr(F, <<>>, List).

-spec prefix(Binary1 :: binary(), Binary2 :: binary()) -> boolean().
prefix(Binary1, Binary2) when binary_part(Binary2, {0, byte_size(Binary1)}) =:= Binary1 -> true;
prefix(_Binary1, _Binary2) -> false.
