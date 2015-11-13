-module(error_m).

-export([ error_monad/0,
          error/1,
          is_error/1,
          fmap/2,
          liftM/1,
          show_error/1
]).

-type error_m() :: {error, bitstring()} | {ok, any()}.

-spec error_monad() -> monad:monad().
error_monad() ->
  monad:make_monad(error_m,
                   fun bind/2,
                   fun return/1).

bind(V = {error, _Reason}, _F) -> V;
bind({ok, Value}, Fun) -> Fun(Value).

return(Value) -> {ok, Value}.

-spec error(Reason :: bitstring()) -> error_m().
error(Reason) -> {error, Reason}.

-spec is_error(Err :: error_m()) -> boolean().
is_error({error, _}) -> true;
is_error(_) -> false.

-spec fmap(Val :: error_m(), fun((FV :: any()) -> any())) -> error_m().
fmap(V={error, _}, _F) -> V;
fmap({ok, Value}, Func) -> {ok, Func(Value)}.

-spec liftM(Func :: fun((Val :: any()) -> any())) -> fun((error_m()) -> error_m()).
liftM(F) -> fun(Val) -> fmap(Val, F) end.

-spec show_error(error_m()) -> string().
show_error({Term, Value}) ->
  binary_to_list(list_to_binary(io_lib:format("~p: ~p", [Term,Value]))).
