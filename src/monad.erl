-module(monad).

-export([make_monad/3,
         fmap/3,
         bind/1,
         return/1,
         id/1,
         join/2,
         sequence/2
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

-spec fmap(monad(), any(), fun((any()) -> any())) -> any().
fmap(Monad, Functor, Func) ->
  Bind = bind(Monad),
  Ret = return(Monad),
  Ret(Bind(Functor, Func)).

-spec join(monad(), any()) -> any().
join(Monad, V) ->
  Bind = bind(Monad),
  Bind(V, fun(A) -> id(A) end).

-spec sequence(monad(), [any()]) -> any().
sequence(Monad, Values) ->
  foo.
