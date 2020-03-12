-module(core_cast_tests).

-include_lib("eunit/include/eunit.hrl").

to_string_test() ->
    ?assertMatch("123", core_cast:to_string(123)),
    ?assertMatch("foo", core_cast:to_string(<<"foo">>)),
    ?assertMatch("foo", core_cast:to_string(foo)),
    ?assertMatch("123", core_cast:to_string("123")).

to_ip_test() ->
    ?assertMatch({1, 1, 1, 1}, core_cast:to_ip("1.1.1.1")),
    ?assertMatch({1, 1, 1, 1}, core_cast:to_ip(<<"1.1.1.1">>)),
    ?assertMatch({1, 1, 1, 1}, core_cast:to_ip({1, 1, 1, 1})).

to_binary_test() ->
    ?assertMatch(<<"foo">>, core_cast:to_binary("foo")),
    ?assertMatch(<<"foo">>, core_cast:to_binary(foo)),
    ?assertMatch(<<"123">>, core_cast:to_binary(123)),
    ?assertMatch(<<"3.14000000000000012434e+00">>, core_cast:to_binary(3.14)),
    ?assertMatch(<<"foo">>, core_cast:to_binary(<<"foo">>)).

to_int_test() ->
    ?assertMatch(123, core_cast:to_int("123")),
    ?assertMatch(123, core_cast:to_int(<<"123">>)),
    ?assertMatch(123, core_cast:to_int(123)).

to_atom_test() ->
    ?assertMatch(foo, core_cast:to_atom(<<"foo">>)),
    ?assertMatch(foo, core_cast:to_atom("foo")),
    ?assertMatch(foo, core_cast:to_atom(foo)).
