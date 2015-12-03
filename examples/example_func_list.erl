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
