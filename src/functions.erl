-module(functions).
-export([curry/2,
         flip/1,
         compose/2
        ]).

% Performs an η-reduction (sometimes called currying) to partially apply
% a single term to an arity-2 function. Additional definitions must be added
% for higher arity functions.
-spec curry(fun((any(), any()) -> any()), any()) -> fun((any()) -> any()).
curry(F, Param1) -> fun (Param2) -> F(Param1, Param2) end.

% Flips the argument of a 2-arity function.  This is useful when you have
% a function that you wish to use with bind and the arguments are in the
% wrong order.
-spec flip(fun((any(), any()) -> any())) -> fun((any(), any()) -> any()).
flip(F) -> fun(A, B) -> F(B, A) end.

% Compose two arity-1 functions by passing the output of the first as the input
% of the second.
-spec compose(fun((any()) -> any()), fun((any()) -> any())) -> fun((any()) -> any()).
compose(F1, F2) -> fun(Input) -> F2(F1(Input)) end.
