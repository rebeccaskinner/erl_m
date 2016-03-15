-module(monad).

-export([ make_monad/3
        , fmap/3
        , bind/1
        , return/1
        , join/2
        , sequence/2
        , sequence_helper/3
        , ap/3
        , co_fmap/3
        , pure/1
        , make_functor/2
        , make_applicative/5
        , make_monad_plus/3
        ]).

%%% Monad Functions

% returnf :: Monad m => a -> m a
-type returnf() :: fun((any()) -> any()).

% bindf :: Monad m => m a -> (a -> m b) -> m b
-type bindf() :: fun((any(), fun((any()) -> any())) -> any()).

%%% MonadPlus Functions

% mzerof :: MonadPlus m => m a
-type mzerof() :: fun(() -> any()).

% mplusf :: MonadPlus m => m a -> m a -> m a
-type mplusf() :: fun((any(), any()) -> any()).

%%% Functor Functions

% fmapf :: Functor f => (a -> b) -> f a -> f b
-type fmapf() :: fun((any()) -> any()).

%%% Applicative Functions

% puref :: Applicative f => a -> f a
-type puref() :: fun((any()) -> any()).

% apf :: Applicative f => f (a -> b) -> f a -> f b
-type apf() :: fun((any(), fun((any()) -> any())) -> any()).

% seqlf :: Applicative f => f a -> f b -> f a
-type seqlf() :: fun((any(), any()) -> any()).

% seqrf :: Applicative f => f a -> f b -> f b
-type seqrf() :: fun((any(), any()) -> any()).

-type monad() :: { InstanceName :: atom(),
                   MonadFunctions :: map()
}.

-type functor() :: { InstanceName :: atom(),
                     FunctorFunctions :: map()
                   }.

-type applicative() :: { InstanceName :: atom(),
                         ApplicativeFunctions :: map()
                       }.

-type monad_plus() :: { InstanceName :: atom(),
                        MonoidFunctions :: map()
                      }.


-spec make_monad(atom(), bindf(), returnf()) -> monad().
make_monad(Name, Bind, Return) ->
  {Name, #{bind => Bind, return => Return}}.

-spec make_functor(atom(), fmapf()) -> functor().
make_functor(Name, Fmap) ->
  {Name, #{fmap => Fmap}}.

-spec make_applicative(atom(), puref(), apf(), seqlf(), seqrf()) -> applicative().
make_applicative(Name, Pure, Ap, SeqL, SeqR) ->
  {Name, #{pure => Pure, ap => Ap, seqL => SeqL, seqR => SeqR}}.

-spec make_monad_plus(atom(), mzerof(), mplusf()) -> monad_plus().
make_monad_plus(Name, MZero, MPlus) ->
  {Name, #{mzero => MZero, mplus => MPlus}}.

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
  Bind(V, fun(A) -> functions:id(A) end).

sequence_helper(M, Current, Carry) ->
  B = bind(M),
  B(Current, fun(Cur) -> [Cur | Carry] end).

-spec sequence(monad(), [any()]) -> any().
sequence(Monad, Values) ->
  ReturnFunc = return(Monad),
  FoldFunc = fun(Current, Carry) -> sequence_helper(Monad, Current, Carry) end,
  ReturnFunc(lists:foldl(FoldFunc, [], Values)).
