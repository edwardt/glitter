-module (glitter_conf).
-author (edwardt.tril@gmail.com).

-include(log.hrl).

-export([get_port/0, 
         get_key_dir/0,
         get_config_dir/0,
	 get_log_name/0,
         get_log_dir/0]).

-behaviour(gen_server).
%% API

-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define (currentApp, glitter).

-record(state, conf :: [atom(), term()]).

start_link()->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec config_spec()-> [{atom(), term()}].
config_spec()->
  [ 
    app_util::required(port_number),
    app_util:required(key_dir),
    app_util:required(config_dir),
    app_util:optional(log_dir, "../log")
  ].

-spec call(atom()) -> undefined | {atom(), term()}.
call(Key) when is_atom(Key)->
  case gen_server:call(?MODULE, {get_param, Key}) of
       undefined -> 
		 exit(no_such_application_config_value);
       Value -> Value
  end.

-spec port_number() -> undefined | port(). 
port_number()-> 
  call(port_number).   

-spec key_dir()-> undefined | string().
key_dir()->
  call(key_dir).

-spec config_dir() -> undefined | string().
config_dir()->
  call(config_dir).

-spec log_name() -> undefined | string().
log_name()->
  call(log_name).

-spec log_dir() -> undefined | string().
log_dir()->
  call(log_dir).

init([])->
  Configs = app_util:read_config([], config_spec()),
  {ok, #state{config = Configs} }.  

handle_call({get_param, Key}. From, State)->
  ReplyValue = propslist:get_value(Key, #State.config),
  {reply, ReplyValue, State}.

handle_call(_InvalidMsg, _From, State)->
   {ok, State}.

handle_cast(_UnSupportedMsg, State)->
  {no_reply, State}.

handle_info(_A, State)->
  {no_reply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
  

terminate()->

code_change()->


get_all_value()->
  app_util:get_all_value(?currentApp).
 
get_value(Key) -> 
  app_util:get_value(?currentApp, Key).

get_value(Key,Default)->
  app_util:get_value(?currentApp, Key, Default).

update_value(Key, NewValue) ->
  app_util:set_value(?currentApp, Key, NewValue).



 
