-module(calc_test).
-include_lib("eunit/include/eunit.hrl").

init_data() ->
    {queue:new(), dict:new()}.

empty_test() ->
    {Arr, Avgs} = init_data(),
    {Arr2, Avgs2, V} = calc:updOnRequest(Arr, Avgs, dummy),
    ?assert(Arr2 == Arr),
    ?assert(Avgs2 == Avgs),
    ?assert(V == 0.0).

report_test() ->
    {Arr, Avgs} = init_data(),
    {Arr2, Avgs2} = calc:updOnReport(Arr, Avgs, metric1, 3.0),
    ?assert(queue:len(Arr2) == 1),
    {{value, {M, _, V}}, _} = queue:out(Arr2),
    ?assert(M == metric1),
    ?assert(V == 3.0),
    ?assert(dict:fetch(metric1, Avgs2) == {3.0, 1}).

report3_test() ->
    {Arr, Avgs} = init_data(),
    {Arr2, Avgs2} = calc:updOnReport(Arr, Avgs, metric1, 3.0),
    {{value, {M, T, V}}, _} = queue:out(Arr2),
    Arr2a = queue:in({M, T - 1000000, V}, Arr),
    {Arr3, Avgs3} = calc:updOnReport(Arr2a, Avgs2, metric1, 4.0),
    {_, Avgs4} = calc:updOnReport(Arr3, Avgs3, metric1, 5.0),
    io:format("dbg: ~p~n", [dict:fetch(metric1, Avgs4)]),
    ?assert(dict:fetch(metric1, Avgs4) == {9.0, 2}).

request_test() ->
    {Arr, Avgs} = init_data(),
    {Arr2, Avgs2} = calc:updOnReport(Arr, Avgs, metric1, 3.0),
    {Arr3, Avgs3} = calc:updOnReport(Arr2, Avgs2, metric1, 5.0),
    {_, _, Res} = calc:updOnRequest(Arr3, Avgs3, metric1),
    ?assert(Res == 4.0).
