-module(state)

-export([ state_monad/0,          
          get/2,
          put/2,
          run/2]).

-type state() :: fun((any(), any()) -> {fun((any(), any()) -> any()), any()}.


