-module(functions).
-export([curry/1,
         uncurry/1,
         beta_reduce/2,
         flip/1,
         compose/2,
         ap/2
        ]).

beta_reduce(F, Param) ->
  fun(Param2) ->
      F(Param, Param2)
  end.

curry(Fun) ->
  fun({A, B}) ->
      Fun(A, B)
  end.

uncurry(Fun) ->
  fun(A, B) ->
      Fun({A, B})
  end.

flip(F) -> fun(A, B) -> F(B, A) end.

ap(F, V) -> F(V).

compose(F1, F2) -> fun(Input) -> F2(F1(Input)) end.
