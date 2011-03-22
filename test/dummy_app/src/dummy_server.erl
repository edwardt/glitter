-module(dummy_server).
-behaviour(gen_server).
-export([start/0,stop/0,set_state/1,get_state/0]).

-export([init/1,terminate/2,code_change/3,
        handle_call/3,handle_cast/2,handle_info/2]).

-define(SERVER, ?MODULE).

-record(state, {no}).

start()->
    Args = [0],
    ServerOpts = [],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, ServerOpts).

stop()->
    gen_server:cast(?SERVER, {stop, normal}).

set_state(NewState) ->
    gen_server:call(?SERVER, {set_state, NewState}).

get_state()->
    gen_server:call(?SERVER, {get_state}).

init(Args)->
    io:format("Args: ~w ~n",[Args]),
    [StartNum|_Rest] = Args,
    io:format("New State ~w ~n ", [#state.no]),
    {ok,  #state{no=StartNum}}.

handle_call({set_state, NewState}, _From, State)->
    {reply, ok, State = #state{no=NewState}};

handle_call({get_state}, _From, State)->
    {reply, {ok,State#state.no}, State}.

handle_cast({stop, Reason}, State)->
    {stop, Reason, State};
handle_cast(Msg, State)->
    {no_reply, Msg, State}.

handle_info(Msg, State)->
    {no_reply, Msg, State}.

terminate(_Why, _State)->
    ok.

code_change(_OldVsn, _NewVsn, _Opts)->
    ok.
