-module(erl_m_app).

-behavior(application).
-export([start/2, stop/1]).

-behavior(supervisor).
-export([init/1]).

start(_, _) ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).

stop(_) -> ok.

init([]) ->
  {ok, {{one_for_one,3,10},[]}}.
