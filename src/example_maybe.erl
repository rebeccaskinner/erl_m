-module(example_maybe).

-export([main/1]).

-define(LEXICAL_ATOMS, #{ one => 1,
                          two => 2,
                          three => 3,
                          four => 4,
                          five => 5,
                          six => 6,
                          seven => 7,
                          eight => 8,
                          nine => 9
                        }).

readNumberish(Num) when is_integer(Num) -> maybe_m:just(Num);
readNumberish(Num) when is_atom(Num) ->
  case maps:is_key(Num, ?LEXICAL_ATOMS) of
    true -> maybe_m:just(maps:get(Num, ?LEXICAL_ATOMS));
    _ -> maybe_m:nothing()
  end;
readNumberish(Num) when is_list(Num) ->
  case string:to_integer(Num) of
    {Num1, _} -> maybe_m:just(Num1);
    _ -> readNumberish(list_to_atom(Num))
  end.

maybeParseArgs(L) when is_list(L) ->
  lists:map(fun readNumberish/1, L);
maybeParseArgs(_) -> [].

maybe_sum(A, B) when is_number(A) andalso is_number(B) -> maybe_m:just(A + B);
maybe_sum(_, _) -> maybe_m:nothing().

foldFunc(Elem, Carry) ->
  BindFunction = monad:bind(maybe_m:maybe_monad()),
  BindFunction(Carry,
               fun(BareCarry) ->
                   BindFunction(Elem,
                                fun(BareElem) ->
                                    maybe_sum(BareCarry, BareElem)
                                end
                               )
               end
              ).

main(Args) ->
  Result = lists:foldl(fun foldFunc/2, maybe_m:just(0), maybeParseArgs(Args)),
  io:format("Result: ~p~n", [Result]).
