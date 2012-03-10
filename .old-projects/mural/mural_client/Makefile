
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
