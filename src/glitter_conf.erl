-module (glitter_conf).
-author (edwardt.tril@gmail.com).

-export([get_value/1, 
         get_value/2,
         get_all_value/0,
         update_value/2]).

-behaviour(gen_server).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define (currentApp, glitter).






get_all_value()->
  app_util:get_all_value(?currentApp).
 
get_value(Key) -> 
  app_util:get_value(?currentApp, Key).

get_value(Key,Default)->
  app_util:get_value(?currentApp, Key, Default).

update_value(Key, NewValue) ->
  app_util:set_value(?currentApp, Key, NewValue).


required(Key) when is_atom(Key) ->
  fun(ConfigKV)->
	%get from in memory or else from config
	Val = case proplist:get_value(Key, ConfigKV) of
		 undefined -> 
			case get_value(Key) of
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

optional(Key, Default) when is_atom(Key) ->
  fun(ConfigKV)->
	%get from in memory or else from config
	Val = case proplist:get_value(Key, ConfigKV, Default) of
		 {ok, Value} -> Value; 
	         _Else -> Default
              end, 
        {Key, Val}
  end.
 
