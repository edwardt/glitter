-module (app_util).
-author (edwardt.tril@gmail.com).

-export([ensure_app_started/1, 
         ensure_app_stopped/1]).

ensure_app_start(AppName) when is_atom(AppName) ->
  case application:start(AppName) of ->
       ok -> ok;
       {error, {alread_started, _}} -> ok;
       {error, ERROR} -> {error, ERROR}
  end.

ensure_app_stop(AppName) when is_atom(AppName) ->
  case application:stop(AppName) of
       ok -> ok;
       {already_stopped, _} -> ok;
       {error, {already_stopped, _}} -> ok;
       {error, ERROR} -> {error, ERROR}
  end.  