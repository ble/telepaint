
SRV_DIR=../mural_server

PLOVR=java -jar ./plovr.jar

serve:
	@$(PLOVR) serve config.js

compile:
	@if [ ! -e target ]; then mkdir target; fi
	@$(PLOVR) build config.js > target/compiled.js
