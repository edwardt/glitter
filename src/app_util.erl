-module (app_util).
-author (edwardt.tril@gmail.com).

-export([ensure_app_start/1, 
         ensure_app_stop/1,
         get_all_value/1,
         get_value/2,
         get_value/3,
         set_value/3]).

ensure_app_start(AppName) when is_atom(AppName) ->
  case application:start(AppName) of 
       ok -> ok;
       {error, {already_started, _}} -> ok;
       {error, ERROR} -> {error, ERROR}
  end.

ensure_app_stop(AppName) when is_atom(AppName) ->
  case application:stop(AppName) of
       ok -> ok;
       {already_stopped, _} -> ok;
       {error, {already_stopped, _}} -> ok;
       {error, ERROR} -> {error, ERROR}
  end.  

get_all_value(App) when is_atom(App)->
  application:get_all_env(App).

get_value(App, Key) when is_atom(Key) ->
  get_value(App, Key, undefined).

get_value(App, Key, Default) when is_atom(Key) ->
  case application:get_env(App, Key) of 
      {ok, Value} ->
           Value;
      _Else ->
           Default
  end.

set_value(App, Key, Value) when is_atom(App), is_atom(Key) ->
  case  application:set_env(App, Key, Value) of
      ok ->  ok;
      {ok , _Val} -> ok;
      {error, Error} -> {error, Error}
  end.