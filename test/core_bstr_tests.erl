-module(core_bstr_tests).

-include_lib("eunit/include/eunit.hrl").

join_test() ->
    ?assertMatch(<<"foobar">>, core_bstr:join([<<"foo">>, <<"bar">>])),
    ?assertMatch(<<"foo:bar">>, core_bstr:join([<<"foo">>, <<"bar">>], <<":">>)),
    ?assertMatch(<<"foo">>, core_bstr:join([<<"foo">>], <<":">>)),
    ?assertMatch(<<>>, core_bstr:join([], <<":">>)).

prefix_test() ->
    ?assertMatch(true, core_bstr:prefix(<<"foo">>, <<"foobar">>)),
    ?assertMatch(false, core_bstr:prefix(<<"bar">>, <<"foobar">>)).
