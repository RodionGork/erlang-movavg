-module(calc).
-export([launch/0, process/2]).

-ifdef(EUNIT).
-export([updOnReport/4, updOnRequest/3]).
-endif.

-define(AVERAGE_PERIOD, 60000).

launch() ->
    spawn(calc, process, [queue:new(), dict:new()]).

curTimeStamp()->
    erlang:system_time(millisecond).

removeOld(Arr, Avgs) ->
    Ts = curTimeStamp(),
    removeOlderThan(Arr, Avgs, Ts - ?AVERAGE_PERIOD).

removeOlderThan(Arr, Avgs, Ts) ->
    Empty = queue:is_empty(Arr),
    {value, {M, T, V}} = case Empty of
        false -> queue:peek(Arr);
        true -> {value, {0, Ts, 0.0}}
    end,
    if
        T >= Ts ->
            {Arr, Avgs};
        true ->
            Arr2 = queue:tail(Arr),
            {PrevV, PrevN} = dict:fetch(M, Avgs),
            Avgs2 = dict:store(M, {PrevV - V, PrevN - 1}, Avgs),
            removeOlderThan(Arr2, Avgs2, Ts)
    end.

updOnReport(Arr, Avgs, Metric, Value) ->
    Ts = curTimeStamp(),
    Arr2 = queue:in({Metric, Ts, Value}, Arr),
    {PrevV, PrevN} = case dict:is_key(Metric, Avgs) of
        true -> dict:fetch(Metric, Avgs);
        false -> {0.0, 0}
    end,
    Avgs2 = dict:store(Metric, {PrevV + Value, PrevN + 1}, Avgs),
    removeOld(Arr2, Avgs2).

updOnRequest(Arr, Avgs, Metric) ->
    {Arr2, Avgs2} = removeOld(Arr, Avgs),
    Exists = dict:is_key(Metric, Avgs2),
    {Sum, Count} = case Exists of
        true -> dict:fetch(Metric, Avgs2);
        false -> {0.0, 1.0}
    end,
    Res = if
        Count > 0 -> Sum / Count;
        true -> 0.0
    end,
    {Arr2, Avgs2, Res}.

process(Arr, Avgs) ->
    receive
        {reported, Metric, Value} ->
            %io:format("Report For ~p: ~p~n", [Metric, Value]),
            {Arr2, Avgs2} = updOnReport(Arr, Avgs, Metric, Value);
        {requested, Metric, PID_req} ->
            {Arr2, Avgs2, Res} = updOnRequest(Arr, Avgs, Metric),
            %io:format("Avg Asked For ~p~n", [Metric]),
            PID_req ! Res
    end,
    process(Arr2, Avgs2).
