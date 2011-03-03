-module (app_util).

-author (edwardt.tril@gmail.com).

-export([ensure_app_start/1, 
         ensure_app_stop/1,
         get_all_value/1,
         get_value/2,
         get_value/3,
         set_value/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


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

%% =========== unit tests ===========
-ifdef(TEST).

app_util_test_()->
  { setup,
    fun setup/0,
    fun cleanup/1,
    [
      fun app_start_test_case/0,
      fun app_already_start_test_case/0,
      fun app_start_undef_app_test_case/0,
      fun app_stop_test_case/0,
      fun app_already_stop_test_case/0,
      fun app_stop_undef_app_test_case/0,
      fun set_appvalue_test_case/0,
      fun set_appvalue_undef_key_test_case/0,
      fun set_appvalue_undef_app_test_case/0
      fun get_all_value_test_case/0,
      fun get_value_test_case/0,
      fun get_value_undef_app_test_case/0,
      fun get_value_undef_key_test_case/0,
      fun get_default_value_test_case/0
    ]
  }.

app_start_test_case() ->
  
 .
app_already_start_test_case()->
 .
app_start_undef_app_test_case()->
 .
app_stop_test_case()->
 .
app_already_stop_test_case()->
 .
app_stop_undef_app_test_case()->
 .
set_appvalue_test_case()->
 .
set_appvalue_undef_key_test_case()->
 .
set_appvalue_undef_app_test_case()->
 .
get_all_value_test_case()->
 .
get_value_test_case()->
 .
get_value_undef_app_test_case()->
 .
get_value_undef_key_test_case()->
 .
get_default_value_test_case()->
 .
-endif.
