
SRV_DIR=../mural_server
JS_DIR=$(SRV_DIR)/priv/www/js

PLOVR=java -jar ./plovr.jar

serve:
	@$(PLOVR) serve config.js

compile:
	@if [ ! -e target ]; then mkdir target; fi
	@$(PLOVR) build config.js > target/compiled.js

push:
	cp target/compiled.js $(JS_DIR)
#all:
#	@$(REBAR) get-deps compile
#
#edoc:
#	@$(REBAR) doc
#
#test:
#	@rm -rf .eunit
#	@mkdir -p .eunit
#	@$(REBAR) skip_deps=true eunit
#
#clean:
#	@$(REBAR) clean
#
#build_plt:
#	@$(REBAR) build-plt
#
#dialyzer:
#	@$(REBAR) dialyze
#
#app:
#	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)
#
