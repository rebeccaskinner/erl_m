-module(lists_extra).
-export([ unfoldl/2,
          unfoldr/2,
          repeat/2,
          intersperse/2,
          intercalate/2
        ]).

-spec unfoldl(any(), fun((any()) -> maybe_m:maybe_m())) -> [any()].
unfoldl(Seed, Func) -> unfoldl(Seed, Func, []).

unfoldl(Carry, Func, Lst) ->
    MaybeResult = Func(Carry),
    case maybe_m:is_just(MaybeResult) of
        true -> {Elem, Carry1} = maybe_m:from_just(MaybeResult),
                unfoldl(Carry1, Func, [Elem | Lst]);
        false -> Lst
    end.

-spec unfoldr(any(), fun((any()) -> maybe_m:maybe_m())) -> [any()].
unfoldr(Seed, Func) ->
    MaybeResult = Func(Seed),
    case maybe_m:is_just(MaybeResult) of
        true ->
            {Elem, Carry1} = maybe_m:from_just(MaybeResult),
            [Elem | unfoldr(Carry1, Func)];
        false -> []
    end.

-spec repeat(any(), integer()) -> [any()].
repeat(Elem, Count) ->
    lists:map(fun(_) -> Elem end, lists:seq(0,Count)).

-spec intersperse(any(), [any()]) -> [any()].
intersperse(Infix, List) ->
    lists:reverse(
      tl(
        lists:reverse(
          lists:foldl(fun(Elem, Acc) -> [Elem | [Infix | Acc]] end, [], List)))).

-spec intercalate([any()], [any()]) -> [any()].
intercalate(Infix,List) ->
    lists:concat(intersperse(Infix, List)).
