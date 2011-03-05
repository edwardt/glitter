REBAR					= ./rebar
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
VERBOSE				        = -v
LOCAL					= write_scripts
RELEASE					= write_release_scripts

.PHONY: deps

all: compile

compile: deps
	@sed -i.bak 's/%%VERSION%%/'"$VERSION"'/g' $(PWD)/src/$(APP).app.src
	@$(REBAR) compile

deps:
	@($(REBAR) get-deps)

rel: all	
	@cd $(PWD); rm -rf rel; mkdir -p rel
	%make boot script	
	@$(REBAR) generate

boot_fixme:
	@(cd ebin; $(ERL) -init_debug -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot $(LOCAL) $(APP) $(VERSION);)

bootRel_fixme:
	@(cd ebin; $(ERL) -init_debug -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot $(RELEASE) $(APP) $(VERSION);)


edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: compile
	@rm -rf .eunit
	@mkdir -p .eunit
	@($(REBAR) skip_deps=true eunit)

build_plt: 
	@$(REBAR) built-plt

dialyzer:
	@$(REBAR) dialyze

get-deps: clean
	@($(REBAR) $(VERBOSE) get-deps)

delete-deps: 
	@($(REBAR) $(VERBOSE) delete-deps)
	
clean:
	@(cd $(PWD); rm -rf ./rel)
	@($(REBAR) $(VERBOSE) clean)

