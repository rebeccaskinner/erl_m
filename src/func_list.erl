-module(func_list).
-export([unit/0,
         cons/2,
         head/1,
         tail/1,
         to_list/1,
         from_list/1,
         map/2,
         return/1,
         reverse/1,
         foldl/3,
         foldl1/2,
         concat/2,
         flatten/1,
         bind/2,
         list_monad/0
        ]).

unit() ->
    fun() -> {} end.

cons(Elem, Lst) ->
    fun() -> {Elem, Lst} end.

head(Lst) ->
    element(1,Lst()).

tail(Lst) ->
    element(2, Lst()).

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
