-module(functions).
-export([curry/1,
         uncurry/1,
         beta_reduce/2,
         flip/1,
         compose/2,
         ap/2
        ]).

beta_reduce(F, Param) ->
  {arity, Arity} = erlang:fun_info(F, arity),
  beta_reduce1([Param], F, Arity).

beta_reduce1(Params, F, Arity) when length(Params) =:= Arity ->
  erlang:apply(F, lists:reverse(Params));
beta_reduce1(Params, F, Arity) ->
  fun(Param) ->
    beta_reduce1([Param | Params], F, Arity)
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
