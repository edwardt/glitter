-module (app_util).

-author (edwardt.tril@gmail.com).

-export([ensure_app_start/1, 
         ensure_app_stop/1,
         get_all_value/1,
         get_value/2,
         get_value/3,
         set_value/3,
         required/2,
	 optional/3]).

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
       {error, {"no such file or directory", App}} ->
               {error, {"no such file or directory", App}};
       {error, {not_started, _}} -> ok;
       {error, ERROR} -> {error, ERROR}
  end.  

get_all_value(App) when is_atom(App)->
  application:get_all_env(App).

get_value(App, Key) when is_atom(Key) ->
  get_value(App, Key, undefined).

get_value(App, Key, Default) when is_atom(Key) ->
  case application:get_env(App, Key) of 
      {ok, Value} ->
           {ok, Value};
      _Else ->
           {ok, Default}
  end.

set_value(App, Key, Value) when is_atom(App), is_atom(Key) ->
  case  application:set_env(App, Key, Value) of
      ok ->  ok;
      {ok , _Val} -> ok;
      {error, Error} -> {error, Error}
  end.


required(App, Key) when is_atom(Key) ->
  fun(ConfigKV)->
	%get from in memory or else from config
	Val = case proplists:get_value(Key, ConfigKV) of
		 undefined -> 
			case get_value(App, Key) of
                             {ok, Value0} -> Value0;                       
			     undefined -> undefined
 	                end;
		 Value -> Value
              end, 
        case Val of
             undefined -> undefined;
             AllVal -> {Key, AllVal}
        end           
  end.

optional(App, Key, Default) when is_atom(Key) ->
  fun(ConfigKV)->
	%get from in memory or else from config
	Val = case proplists:get_value(Key, ConfigKV, Default) of
		 {ok, Value} -> Value; 
	         _Else -> 
                       case get_value(App, Key, Default) of
                            {ok, Value0} -> Value0;
                            _Else -> Default
                       end   
              end, 
        {Key, Val}
  end.
 

%% =========== unit tests ===========
-ifdef(TEST).

app_util_test_()->
  { setup,
    fun setup/0,
    fun cleanup/1,
    [
      {"Ensure app start from clean", fun app_start_test_case/0},
      fun app_already_start_test_case/0,
      fun app_start_undef_app_test_case/0,
      fun app_stop_test_case/0,
      fun app_already_stop_test_case/0,
      fun app_stop_undef_app_test_case/0,
      fun set_appvalue_test_case/0,
      fun set_appvalue_undef_key_test_case/0,
      fun set_appvalue_undef_app_test_case/0,
      fun get_all_value_test_case/0,
      fun get_value_test_case/0,
      fun get_value_undef_app_test_case/0,
      fun get_value_undef_key_test_case/0,
      fun get_value_or_default_undef_key_test_case/0,
      fun required_key_from_memory/0,
      fun required_key_from_env/0,
      fun required_undefine_key_from_memory/0,
      fun required_key_no_val_from_memory/0,
      fun optional_key_from_memory/0,
      fun optional_key_from_env/0
    ]
  }.
setup() ->
 ok.

cleanup(_State) -> 
 ok.
 
app_start_test_case() ->
  ok = ensure_app_stop(sasl),  
  Ret = ensure_app_start(sasl),
  ?assertMatch(ok, Ret).
  
app_already_start_test_case()->
  ok = ensure_app_stop(sasl),
  ok = ensure_app_start(sasl),
  Ret1 = ensure_app_start(sasl),
  ?assertMatch(ok, Ret1).
 
app_start_undef_app_test_case()->
  Ret = ensure_app_start(undefined),
  io:format("Return value: ~p",[Ret]),
  {error, {"no such file or directory", _App}} = Ret.
  % ?assert(ok = ({error,_} = Ret)).
  % ?assertError({"no such file or directory","undefined.app"}, Ret).
 
app_stop_test_case()->
  ok = ensure_app_start(sasl),
  Ret0 = ensure_app_stop(sasl),
  ?assertEqual(ok, Ret0).
 
app_already_stop_test_case()->
  ok = ensure_app_start(sasl),
  ok = ensure_app_stop(sasl),
  Ret1 = ensure_app_stop(sasl),
  ?assertEqual(ok, Ret1).

%TODO behavirou not symmetrical with start. 
app_stop_undef_app_test_case()->
  Ret = ensure_app_stop(undefined),
  ?assertMatch(ok, Ret).

set_appvalue_test_case()->
  Ret = set_value(testapp, testkey, test_val),
  ?assertEqual(ok, Ret),
  Ret0 = application:get_env(testapp,testkey),
  ?assertMatch({ok,test_val},Ret0).
 
set_appvalue_undef_key_test_case()->
  Ret = set_value(testapp, undefined, test_val),
  % TODO this has to deal with becasue it allows key namesd as undefined 
  %?assertEqual(undefined, Ret).
  io:format("You can name a key undefined with even undefined as value."),
  ?assertEqual(ok, Ret).
 
set_appvalue_undef_app_test_case()->
  Ret = set_value(undefined,undefined,test),
  io:format("You can actually make an application with name undefined, key undefined"),
  ?assertEqual(ok ,Ret).
 
get_all_value_test_case()->
  ok = set_value(testapp2, testkey, testvalue),
  ok = set_value(testapp2, testkey1, testvalue1),
  Ret = lists:reverse(get_all_value(testapp2)),
  ?assertMatch([{testkey, testvalue}, {testkey1, testvalue1}],
              Ret).
 
get_value_test_case()->
  ok = set_value(testapp1,testkey1,testvalue),
  Ret = get_value(testapp1, testkey1),
  ?assertMatch({ok,testvalue}, Ret).
 
get_value_undef_app_test_case()->
  Ret = get_value(undefined,somekey),
  ?assertEqual(undefined, Ret) .
 
get_value_undef_key_test_case()-> 
 Ret = get_value(testapp,somekey),
 ?assertEqual(undefined, Ret) .

get_value_or_default_undef_key_test_case()->
  Ret = get_value(testapp,keyfake,defaultval),
  ?assertMatch({ok,defaultVal}, Ret).

required_key_from_memory()->
  InMemory = {test1, {key1, value2}},
  Ret = required(key1, InMemory),
  ?assertEqual(value2, Ret).

required_undefine_key_from_memory()->
  InMemory = {test1, {key1, value2}},
  Ret = required(key2, InMemory),
  ?assertEqual({key2, undefined}, Ret).

required_key_no_val_from_memory()->
  InMemory = {test1, {key1}},
  Ret = required(key1, InMemory),
  ?assertEqual({key1, undefined}, Ret).

required_key_from_env()->
  ok = application:set_value(testtest, testkey, testval),
  Ret = required(testtest, testkey),
  ?assertEqual(testval, Ret).

optional_key_from_memory()->
  InMemory = {test, {keytest}},
  Ret = optional(testAppOpt0, testkeyOpt0, InMemory),
  ?assertEqual(defaulttest, Ret). 

optional_key_from_env()->
  ok = application:set_value(testApp0pt1, testkeyOpt1, testval1),
  Ret = optional(testAppOpt1, testkeyOpt2, defaultOptVal1),
  ?assertEqual(defaultOptVal1, Ret). 

-endif.
