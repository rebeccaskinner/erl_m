-module(example_func_list).
-compile([debug_info, export_all]).

example_intercalate() ->
  ABC = func_list:from_list("abc"),
  DEF = func_list:from_list("def"),
  GHI = func_list:from_list("ghi"),
  FullList = func_list:cons(ABC,(func_list:cons(DEF,func_list:return(GHI)))),
  Infix = func_list:from_list("<====>"),
  func_list:to_list(func_list:intercalate(Infix, FullList)).


countdown(0) ->
    maybe_m:nothing();
countdown(Num) ->
    maybe_m:just({Num, Num - 1}).

unfold_countdown(StartWith) ->
  Countdown = func_list:unfoldr(StartWith, fun countdown/1),
  func_list:to_list(Countdown).

monadic_list_example(Lst) ->
  F = fun(A) ->
         func_list:repeat(3, A)
      end,
  M = func_list:list_monad(),
  B = monad:bind(M),
  L = B(func_list:from_list(Lst), F),
  func_list:to_list(L).  

lazy_list_example(Lst) ->
  F = fun(A, B) ->
          io:format("(~p, ~p) -> {~p, ~p}~n", [A,B,A,B]),
          {A, B}
      end,
  func_list:foldl1(F, func_list:from_list(Lst)).
  
