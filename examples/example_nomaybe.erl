-module(example_nomaybe).
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

readNumberish(Num) when is_integer(Num) -> Num;
readNumberish(Num) when is_atom(Num) ->
  case maps:is_key(Num, ?LEXICAL_ATOMS) of
    true -> maps:get(Num, ?LEXICAL_ATOMS);
    _ -> error
  end;
readNumberish(Num) when is_list(Num) ->
  case string:to_integer(Num) of
    {Num1, _} -> Num1;
    _ -> readNumberish(list_to_atom(Num))
  end.

addValues(A, B) when is_number(A) andalso is_number(B) -> A + B;
addValues(_, _) -> error.

getArgs(L) when is_list(L) ->
  lists:map(fun readNumberish/1, L);
getArgs( _ ) -> [].

foldFunc(_, error) -> error;
foldFunc(Value, Carry) -> addValues(Value, Carry).

main(Args) ->
  Sum = lists:foldl(fun foldFunc/2, 0, getArgs(Args)),
  io:format("Result: ~p~n",[Sum]).
