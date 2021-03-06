-module(glitter_tests).

-include_lib("eunit/include/eunit.hrl").
-define(TEST_FILE, "../test/fake-admin/conf/test.conf").

setup() ->
  ok = application:set_env(glitter, config_file, "../test/example.conf"),
  glitter:start_link().

writable_setup() ->
  file:delete(?TEST_FILE),
  ok = application:set_env(glitter, config_file, ?TEST_FILE),
  glitter:start_link().

teardown(_) ->
  glitter:stop(),
  timer:sleep(100),
  file:delete("../test/test.conf").

%% Starting with a decent config file, that we don't write over.
readonly_glitter_test_() ->
  {inorder,
   {setup,
    fun setup/0,
    fun teardown/1,
    [
      {"Asserting auser has RW access to a repo", fun list_repos/0},
      {"Asserting repo not present", fun has_git_repos/0}
    ]
   }
  }.

%% reading and writing a config file for testing
readwrite_glitter_test_() ->
  {inorder,
   {setup,
    fun writable_setup/0,
    fun teardown/1,
    [
     {"Creating multiple repos",fun add_repos/0},
     {"Removing a created repo", fun remove_repos/0},
     {"Add user to a repo", fun add_user_to_repos/0},
     {"Add user to an undefined repo", fun add_user_to_repos_that_doesnt_exist/0},
     {"Remove a user from a repo", fun remove_user_from_repos/0},
     {"Create membership groups and list groups", fun set_and_list_groups/0},
     {"Just create a user to system", fun add_user/0},
     {"Pass config to system test", fun config_arg_test/0}
    ]
   }
  }.

list_repos() ->
  [Arepo|_] = glitter:list_repos(),
  ?assertEqual({"arepo", [{"auser","RW+"}]}, Arepo),
  passed.

has_git_repos() ->
  ?assert(glitter:has_git_repos("arepo")),
  ?assertNot(glitter:has_git_repos("norepo")),
  passed.

add_repos() ->
  ?assertNot(glitter:has_git_repos("first")),
  ok = glitter:add_repos("first"),
  ?assert(glitter:has_git_repos("first")),
  ?assertEqual(1, length(glitter:list_repos())),

  ?assertNot(glitter:has_git_repos("second")),
  glitter:add_repos("second"),
  ?assert(glitter:has_git_repos("second")),
  ?assertEqual(2, length(glitter:list_repos())),

  glitter:add_repos("second"),
  ?assertEqual(2, length(glitter:list_repos())),
  passed.

remove_repos() ->
  ?assertEqual(2, length(glitter:list_repos())),
  ?assert(glitter:has_git_repos("second")),
  ok = glitter:remove_repos("second"),
  ?assertNot(glitter:has_git_repos("second")),
  ?assertEqual(1, length(glitter:list_repos())).

add_user_to_repos() ->
  ?assert(glitter:has_git_repos("first")),
  glitter:add_user_to_repos({"jdunphy", "RW+"}, "first"),
  {config, Repos, _} = conf_reader:parse_file(?TEST_FILE),
  First = proplists:get_value("first", Repos),
  ?assertEqual([{"jdunphy", "RW+"}], First).

add_user_to_repos_that_doesnt_exist() ->
  ?assertNot(glitter:has_git_repos("new")),
  glitter:add_user_to_repos({"newguy", "RW+"}, "new"),
  {config, Repos, _} = conf_reader:parse_file(?TEST_FILE),
  New = proplists:get_value("new", Repos),
  ?assertEqual([{"newguy", "RW+"}], New).

remove_user_from_repos() ->
  glitter:add_user_to_repos({"a", "RW+"}, "removetest"),
  glitter:add_user_to_repos({"b", "RW+"}, "removetest"),
  {config, Repos, _} = conf_reader:parse_file(?TEST_FILE),
  Orig = proplists:get_value("removetest", Repos),
  ?assertEqual([{"b", "RW+"},{"a", "RW+"}], Orig),
  glitter:remove_user_from_repos("a", "removetest"),

  {config, UpdatedRepos, _} = conf_reader:parse_file(?TEST_FILE),
  UpdatedUsers = proplists:get_value("removetest", UpdatedRepos),
  ?assertEqual([{"b", "RW+"}], UpdatedUsers).

add_user() ->
  PubkeyDir = filename:dirname(?TEST_FILE) ++ "/../keydir",
  PubkeyFile = filename:join([PubkeyDir, "username.pub"]),

  glitter:add_user("username", "pubkeypubkey"),

  true = filelib:is_file(PubkeyFile),
  {ok, Pubkey} = file:read_file(PubkeyFile),
  "pubkeypubkey" = binary_to_list(Pubkey),
  passed.

set_and_list_groups() ->
  glitter:set_group("@agroup", ["a", "b"]),
  Groups = glitter:list_groups(),
  ?assertEqual(["a","b"], proplists:get_value("@agroup", Groups)),

  glitter:set_group("@agroup", ["d", "b"]),
  ResetGroups = glitter:list_groups(),
  ?assertEqual(["d","b"], proplists:get_value("@agroup", ResetGroups)),
  ?assertEqual(1, length(glitter:list_groups())),

  glitter:set_group("@bgroup", ["a", "b"]),
  ?assertEqual(["a","b"], proplists:get_value("@bgroup", glitter:list_groups())),
  ?assertEqual(2, length(glitter:list_groups())),
  passed.

config_arg_test() ->
  glitter:start_link([{config_file, "../test/argtest.conf"}]),
  [{"argtest", _}] = glitter:list_repos(),
  teardown(go),
  passed.
