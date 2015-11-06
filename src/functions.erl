-module(functions).
-export([curry/2,
         flip/1,
         compose/2
        ]).

-spec curry(fun((any(), any()) -> any()), any()) -> fun((any()) -> any()).
curry(F, Param1) -> fun (Param2) -> F(Param1, Param2) end.

-spec flip(fun((any(), any()) -> any())) -> fun((any(), any()) -> any()).
flip(F) -> fun(A, B) -> F(B, A) end.

-spec compose(fun((any()) -> any()), fun((any()) -> any())) -> fun((any()) -> any()).
compose(F1, F2) -> fun(Input) -> F2(F1(Input)) end.
