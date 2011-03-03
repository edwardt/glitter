LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= $(shell cat VERSION | tr -d '\n')
CC					= erlc
ERL					= erl
EBIN					= ebin
INCLUDE_DIRS 				= include
CFLAGS					= +debug_info -W0 -I $(INCLUDE_DIRS) -pa $(EBIN) -I gen-erl/
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
DEPS_DIR 				= deps
EBIN_DIRS				= $(wildcard $(DEPS_DIR)/*/ebin) $(wildcard include/*/ebin)
APP					= glitter

.PHONY: deps

all: compile boot

compile: deps
	@(./rebar compile)

deps:
	@(./rebar get-deps)

boot:
	(cd ebin; $(ERL) -init_debug -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts $(APP) $(VERSION);)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: compile
	@(./rebar skip_deps=true eunit)

distclean: clean
	@(./rebar delete-deps)
	
clean:
	@(./rebar clean)
