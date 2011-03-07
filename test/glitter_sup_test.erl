-module(glitter_sup_test).
-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test_()->
  {
    inorder,
    {
      setup,
      fun setup/0,
      [
         %fun start_server_test/0
        {"Cleanly start server", fun start_server_test/0}
      ],
      fun teardown/0}
  }.

setup()->
  ok.

teardown()->
  ok.

ensure_not_started(App) when is_atom(App) ->
  case application:stop(App) of
     ok -> ok;
     {error,{not_started,App}} -> ok;
     Else -> Else
  end.

ensure_not_loaded(App) when is_atom(App)->
  case application:loaded(App) of
      ok -> ok;
      {error,{not_loaded,sasl}} -> ok;
      Else -> Else    
  end.

start_server_test()->
  App = glitter,
  ok = ensure_not_started(App),
  ok = ensure_not_loaded(App),
 % {ok, PID}  = glitter_sup:start_link(),
 % true = erlang:exit(PID, kill),
  ok.


%% Add more tests on different startup configs




-endif.
