-module(glitter_sup_tests).
-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test_()->
  {"normal sup startup test",
    setup,
    fun setup/0,
    fun()->
    end,
    fun teardown/0
  }.

%% Add more tests on different startup configs




-endif.
