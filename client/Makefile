
SRV_DIR=../mural_server

PLOVR=java -jar ./plovr.jar

#serve:
#	@$(PLOVR) serve --listen localhost config.js

#compile:
#	@if [ ! -e target ]; then mkdir target; fi
#	@$(PLOVR) build config.js > target/compiled.js
clean:
	rm target/*

target/sheet-test.js:
	cp configs/sheet-test.js src/config.js
	@$(PLOVR) build src/config.js > target/sheet-test.js
	rm src/config.js

deploy: target/sheet-test.js
	cp target/*.js ../deploy/static/js/

