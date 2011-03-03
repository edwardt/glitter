-module (glitter_conf).
-author (edwardt.tril@gmail.com).

-export([get_value/1, 
         get_value/2,
         get_all_value/0,
         update_value/2]).

-define (currentApp, glitter).


get_all_value()->
  app_util:get_all_value(?currentApp).
 
get_value(Key) -> 
  app_util:get_value(?currentApp, Key).

get_value(Key,Default)->
  app_util:get_value(?currentApp, Key, Default).

update_value(Key, NewValue) ->
  app_util:set_value(?currentApp, Key, NewValue).