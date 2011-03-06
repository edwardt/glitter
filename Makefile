#### TODO PLEASE CHECK RIAKS BUILD FLOW, WOULD LIKE TO INCORPORATE THEIR IDEAS FORM GIT 
#### GIT TAGGING ang REVISION ....

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


.PHONY: deps

all: compile

compile: deps
	@cd $(PWD); cd ./src; sed -i.bak 's/VERSION/'"$(VERSION)"'/g' $(APP).app.src
	@cd $(PWD); $(REBAR) compile
	@cd $(PWD); cd ./src; sed -i.bak 's/'"$(VERSION)"'/VERSION/g' $(APP).app.src
	
deps:
	@($(REBAR) get-deps)

rel: all	
	@cd $(PWD); rm -rf rel/$(APP)
	@cd $(PWD); cd ./rel; sed -i.bak 's/VERSION/'"$(VERSION)"'/g' reltool.config
	@$(REBAR) -v -f generate
	@cd $(PWD); cd ./rel; sed -i.bak 's/'"$(VERSION)"'/VERSION/g' reltool.config

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

clean_rel:
	@(cd $(PWD); rm -rf ./rel)	

clean: clean_rel
	@($(REBAR) $(VERBOSE) clean)

