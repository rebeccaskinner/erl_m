-module(either).

-export([ either_monad/0,
          left/1,
          right/1,
          is_left/1,
          is_right/1,
          from_either/1,
          force_either/1,
          to_error/1
]).

-type either_m() :: {left | right, any()}.

-spec either_monad() -> monad:monad().
either_monad() ->
  monad:make_monad(either_m,
                   fun bind/2,
                   fun right/1).

-spec right(any()) -> either_m().
right(Value) -> {right, Value}.

-spec left(any()) -> either_m().
left(Value) -> {left, Value}. 

-spec is_left(either_m()) -> boolean().
is_left({left, _}) -> true;
is_left(_) -> false.

-spec is_right(either_m()) -> boolean().
is_right({right, _}) -> true;
is_right(_) -> false.

-spec from_either(either_m()) -> any().
from_either({right, Value}) -> Value;
from_either(_) -> throw({type_exception, "From Either: Not a RIGHT Value"}).

-spec force_either(either_m()) -> any().
force_either({_, Value}) -> Value.

-spec to_error(either_m()) -> error_m:error_m().
to_error({left, V}) ->
  error_m:error(io:format("Left ~p",[V]));
to_error({right, V}) ->
  R = monad:return(error_m:error_monad()),
  R(V).

bind(V = {left, _Reason}, _F) -> V;
bind({right, Value}, Fun) -> Fun(Value).
