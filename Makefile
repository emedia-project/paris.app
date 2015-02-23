REBAR       = ./rebar
VERSION     = $(shell ./tag)
RM_RF       = rm -rf
RM_F        = rm -f
MKDIR_P     = mkdir -p 
CP_R        = cp -r 

.PHONY: compile get-deps

all: build

build: compile
	@$(REBAR) escriptize

release: build
ifeq ($(VERSION),ERROR)
	@echo "**> Can't find version!"
else
	@echo "==> Release version $(VERSION)"
	git clone git@github.com:emedia-project/paris.app.wiki.git
	cp paris paris.app.wiki/paris
	cd paris.app.wiki; git commit -am "New release $(VERSION)"; git push origin master
	rm -rf paris.app.wiki
	git commit -am "Release version $(VERSION)"
	git tag $(VERSION)
	git push origin master --tags
endif

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps

test: get-deps
	@$(REBAR) skip_deps=true eunit

doc:
	@mkdir doc
	@cp _doc/* doc
	$(REBAR) skip_deps=true doc

dev:
	@erl -pa ebin include apps/*/ebin apps/*/include deps/*/ebin deps/*/include #-config sys.config

analyze: checkplt
	@$(REBAR) skip_deps=true dialyze

buildplt:
	@$(REBAR) skip_deps=true build-plt

checkplt: buildplt
	@$(REBAR) skip_deps=true check-plt

cleaninstall: installclean install

installclean:
	@$(RM_RF) ~/.paris
	@$(RM_F) ~/.rebar/templates/paris*

install: build
	@$(MKDIR_P) ~/.paris/plugins
	@$(MKDIR_P) ~/.rebar/templates
	@$(CP_R) rebar_templates/* ~/.rebar/templates
	@$(CP_R) plugins/* ~/.paris/plugins

