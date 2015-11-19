-module(example_lists).

-export([unfold_countdown/1, countdown/1]).

countdown(0) ->
    maybe_m:nothing();
countdown(Num) ->
    maybe_m:just({Num, Num - 1}).

unfold_countdown(StartWith) ->
    Countdown = lists_extra:unfoldr(StartWith, fun countdown/1),
    io:format("~p", [Countdown]).
