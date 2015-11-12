-module(example_maybe).

-export([main/1]).

maybeParseArgs([]) -> maybe_m:nothing();
maybeParseArgs(Args) -> maybe_m:just((Args)).

maybe_sum(A, B) when is_number(A) andalso is_number(B) -> maybe_m:just(A + B);
maybe_sum(_, _) -> maybe_m:nothing().

main(Args) ->
  Nums = maybeParseArgs(Args),
  JoinMaybe = functions:curry(fun monad:join/2, maybe_m:maybe_monad()),
  BindMaybe = monad:bind(maybe_m:maybe_monad()),
  FoldFunc = fun(Value, MaybeCarry) ->
                 MaybeSum1 = functions:curry(fun maybe_sum/2, Value),
                 BindMaybe(MaybeCarry, MaybeSum1)
             end,
  FmapFunc = fun(NumList) -> lists:foldl(FoldFunc, maybe_m:just(0), NumList) end,
  Result = monad:fmap(maybe_m:maybe_monad(), Nums, FmapFunc),
  io:format("Result: ~p~n", [JoinMaybe(Result)]).
