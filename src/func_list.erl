-module(func_list).
-export([ unit/0
        , cons/2
        , head/1
        , tail/1
        , to_list/1
        , from_list/1
        , map/2
        , return/1
        , reverse/1
        , foldl/3
        , foldl1/2
        , concat/2
        , flatten/1
        , bind/2
        , list_monad/0
        , length/1
        , null/1
        , intersperse/2
        , intercalate/2
        , repeat/2
        , cycle/2
        , unfoldr/2
        , zipWith/3
        , zip/2
        , maybe_head/1
        , maybe_tail/1
        ]).

unit() ->
    fun() -> {} end.

cons(Elem, Lst) ->
    fun() -> {Elem, Lst} end.

head(Lst) ->
    element(1,Lst()).

tail(Lst) ->
    element(2, Lst()).

maybe_head(List) ->
  case null(List) of
    true -> maybe_m:nothing();
    false -> maybe_m:just(head(List))
  end.

maybe_tail(List) ->
  case null(List) of
    true -> maybe_m:nothing();
    false -> maybe_m:just(tail(List))
  end.

map(Fun, Lst) ->
    case Lst() of
        {} -> unit();
        {Elem, Rest} -> fun() -> {Fun(Elem), map(Fun, Rest)} end
    end.

reverse(Lst) -> reverse(unit(), Lst).

foldl(Func, Acc, List) ->
    case List() of
        {} -> Acc;
        {Head, Rest} -> foldl(Func, Func(Head, Acc), Rest)
    end.

foldl1(Func, List) ->
    {Head, Rest} = List(),
    foldl(Func, Head, Rest).

concat(List1, List2) ->
    foldl(fun cons/2, List2, reverse(List1)).

flatten(ListOfLists) ->
    foldl(fun concat/2, unit(), reverse(ListOfLists)).

length(List) ->
  foldl(fun(_, Acc) -> Acc + 1 end, 0, List).

cycle(Count, List) when Count =< 1 -> List;
cycle(Count, List) -> concat(List, cycle(Count - 1, List)).

repeat(Count, Elem) -> cycle(Count, return(Elem)).

unfoldr(Seed, Func) ->
  MaybeResult = Func(Seed),
  case maybe_m:is_just(MaybeResult) of
    true ->
      {Elem, Carry1} = maybe_m:from_just(MaybeResult),
      cons(Elem, unfoldr(Carry1, Func));
    false -> unit()
  end.

null(List) ->
  case List() of
    {} -> true;
    _ -> false
  end.

intersperse(Infix, List) ->
  tail(
    reverse(
      foldl(fun(Elem, Acc) -> cons(Elem, cons(Infix, Acc)) end, unit(), List))).

intercalate(Infix, List) ->
  flatten(intersperse(Infix, List)).

zipWith(Func, List1, List2) ->
  case {List1(), List2()} of
    {{}, _} -> unit();
    {_, {}} -> unit();
    {{Head1, Rest1}, {Head2, Rest2}} -> 
      cons(Func(Head1, Head2), zipWith(Func, Rest1, Rest2))
  end.

zip(List1, List2) ->
  F = fun(A, B) -> {A, B} end,
  zipWith(F, List1, List2).

% Monadic Functions
list_monad() -> monad:make_monad(list, fun bind/2, fun return/1).
return(Elem) -> cons(Elem, unit()).
bind(List, Func) -> flatten(map(Func, List)).

% Utility Functions
to_list(Lst) ->
    case Lst() of
        {} -> [];
        {Elem, Rest} -> [Elem | to_list(Rest)]
    end.

from_list(Lst) ->
    lists:foldl(fun cons/2, unit(), lists:reverse(Lst)).

% Internal Functions
reverse(Carry, Lst) ->
    case Lst() of
        {} -> Carry;
        {Elem} -> {Elem};
        {Elem, Rest} -> reverse(cons(Elem, Carry), Rest)
    end.
