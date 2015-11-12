-module(monad).

-export([make_monad/3,
         fmap/3,
         bind/1,
         return/1,
         id/1,
         join/2,
         sequence/2,
         sequence_helper/3,
         ap/3,
         co_fmap/3,
         pure/1
        ]).

-type returnf() :: fun((any()) -> any()).
-type bindf() :: fun((any(), fun((any()) -> any())) -> any()).
-type monad() :: { InstanceName :: atom(),
                   MonadFunctions :: map()
}.

-spec id(any()) -> any().
id(V) -> V.

-spec make_monad(atom(), bindf(), returnf()) -> monad().
make_monad(Name, Bind, Return) ->
  {Name, #{bind => Bind, return => Return}}.

-spec bind(monad()) -> bindf().
bind({_, #{bind := Bind}}) ->
  Bind.

-spec return(monad()) -> returnf().
return({_, #{return := Return}}) ->
  Return.

-spec pure(monad()) -> returnf().
pure(M) -> return(M).

% Apply a function two a value inside of a Functor/Monadic Context
-spec fmap(monad(), any(), fun((any()) -> any())) -> any().
fmap(Monad, Functor, Func) ->
  Bind = bind(Monad),
  Ret = return(Monad),
  Ret(Bind(Functor, Func)).

% Take a Function
-spec ap(monad(), any(), any()) -> any().
ap(Monad, FunctorFunc, FunctorValue) ->
  Bind = bind(Monad),
  Bind(FunctorFunc,
       fun(BareFunc) ->
           fmap(Monad, FunctorValue, BareFunc)
       end).

-spec co_fmap(monad(), any(), any()) -> any().
co_fmap(Monad, FunctorFunc, BareValue) ->
  R = return(Monad),
  ap(Monad, FunctorFunc, R(BareValue)).

-spec join(monad(), any()) -> any().
join(Monad, V) ->
  Bind = bind(Monad),
  Bind(V, fun(A) -> id(A) end).

sequence_helper(M, Current, Carry) ->
  B = bind(M),
  B(Current, fun(Cur) -> [Cur | Carry] end).

-spec sequence(monad(), [any()]) -> any().
sequence(Monad, Values) ->
  ReturnFunc = return(Monad),
  FoldFunc = fun(Current, Carry) -> sequence_helper(Monad, Current, Carry) end,
  ReturnFunc(lists:foldl(FoldFunc, [], Values)).
