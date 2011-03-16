-module (glitter_counter).

-behaviour (gen_server).

-export([create_app/1,
	 update_app/1,
	 delete_app/1, 
	 create_user/1,
	 update_user/1,
	 delete_user/1]).

-export([init/1,
 	 terminate/2,
	 code_change/3,
	 handle_cast/2,
	 handle_info/2,
	 handle_call/3]).



