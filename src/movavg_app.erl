-module(movavg_app).
-export([start/0]).

start() ->
    PID_calc = calc:launch(),
    mainLoop(PID_calc).

mainLoop(PID_calc)->
    {ok, [A, B]} = io:fread("", "~s~s"),
    if
        A == "!" ->
            erlang:halt();
        A == "?" ->
            V = average(PID_calc, list_to_binary(B)),
            io:format("~p~n", [V]);
        true ->
            report(PID_calc, list_to_binary(A), list_to_float(B))
    end,
    mainLoop(PID_calc).

average(PID_calc, Metric) ->
    PID_calc ! {requested, Metric, self()},
    receive
        V -> V
    end.

report(PID_calc, Metric, Value) ->
    PID_calc ! {reported, Metric, Value}.
