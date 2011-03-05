%%%
%%% Make .rel and boot scripts 
%%%
%%% Usage:
%%% cd ebin; erl -pa . -noshell -run make_boot write_scripts app version [app version]... 
%%% 
%%% e.g.:
%%% cd ebin; erl -pa . -noshell -run make_boot write_scripts hermes "0.0.2" stoplight "0.0.1"
%%%
-module(make_boot).
-export([write_scripts/1, write_release_scripts/1]).

write_scripts(A) ->
    write_scripts(A, local).

write_release_scripts(A) ->
    write_scripts(A, release).

write_scripts(A, Dest) -> 
  Args = pair_up(A),
  [Primary|Others] = Args,
  {Name, Version} = Primary,

  io:format("write ~w scripts for ~p~n", [Dest, Name]),
  
  Erts = erlang:system_info(version),
  ok = application:load(sasl),
  ok = application:load(os_mon),
  ok = application:load(crypto),
  ok = application:load(ssl),

  {value, {kernel, _, Kernel}} = lists:keysearch(kernel, 1,
          application:loaded_applications()),
  {value, {stdlib, _, Stdlib}} = lists:keysearch(stdlib, 1,
          application:loaded_applications()),
  {value, {sasl, _, Sasl}} = lists:keysearch(sasl, 1,
          application:loaded_applications()),
  {value, {os_mon, _, Osmon}} = lists:keysearch(os_mon, 1,
          application:loaded_applications()),
 % {value, {crypto, _, Crypto}} = lists:keysearch(crypto, 1,
 %         application:loaded_applications()),
 % {value, {ssl, _, Ssl}} = lists:keysearch(ssl, 1,
 %         application:loaded_applications()),


  Rel = "{release, {\"~s\", \"~s\"}, {erts, \"~s\"}, ["
        "{kernel, \"~s\"}, {stdlib, \"~s\"}, {sasl, \"~s\"}, 
         {os_mon, \"~s\"}, 
         {~s, \"~s\"}~s]}.",
 
  OtherApps = lists:foldl(fun(Elem, AccIn) ->
              {N1, V1} = Elem, 
              AccIn ++ io_lib:format(", {~p, ~p} ~n", [list_to_atom(N1), V1])
      end, "", Others),

  Lowername        = string:to_lower(Name),
  LowernameVersion = string:to_lower(Name ++ "-" ++ Version),

  Filename = lists:flatten(LowernameVersion ++ ".rel"),
  io:format("Writing to ~p (as ~s) ~n", [Filename, Lowername]),
  
  io:format("XXX ~n"), 
  {ok, Fs} = file:open(Filename, [write]),
  io:format("YYY ~n"),
  io:format(Fs, Rel, [Name, Version, Erts, Kernel, Stdlib, 
            Sasl, Osmon, Lowername, Version, OtherApps]),
  io:format("ZZZ ~n"),
  file:close(Fs),
  io:format("sasd ~n"),
  case Dest of
      local   ->  
          io:format("Making boot script for local distribution"),
	  systools:make_script(LowernameVersion, [local]);
      release -> 
          io:format("Making release boot script for release distribution"),
          systools:make_script(LowernameVersion, [{path, ["deps/*/ebin"]}]),
          systools:make_tar(LowernameVersion)
  end,
  io:format("halt ~n"),
  halt().

pair_up([A, B | Tail]) ->
    [{A,B} | pair_up(Tail)];
pair_up([]) ->
    [].
