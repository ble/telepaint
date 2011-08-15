
PLOVR=java -jar ./plovr.jar
STATIC_DIR=../server/priv/www/static-compiled

#serve:
#	@$(PLOVR) serve --listen localhost config.js

#compile:
#	@if [ ! -e target ]; then mkdir target; fi
#	@$(PLOVR) build config.js > target/compiled.js


sheet-test-debug:
	cp configs/sheet-test.js src/config.js
	@$(PLOVR) serve src/config.js --listen 127.0.0.1 --port 9810

snowflake-test-debug:
	cp configs/snowflake-test.js src/config.js
	@$(PLOVR) serve src/config.js --listen 127.0.0.1 --port 9810

blizzard-test-debug:
	cp configs/blizzard-test.js src/config.js
	@$(PLOVR) serve src/config.js --listen 127.0.0.1 --port 9810

foo-debug:
	cp configs/foo.js src/config.js
	@$(PLOVR) serve src/config.js --listen 127.0.0.1 --port 9810

clean:
	rm target/*

target/sheet-test.js:
	cp configs/sheet-test.js src/config.js
	@$(PLOVR) build src/config.js > target/sheet-test.js
	rm src/config.js

target/snowflake-test.js:
	cp configs/snowflake-test.js src/config.js
	@$(PLOVR) build src/config.js > target/snowflake-test.js
	rm src/config.js

target/blizzard-test.js:
	cp configs/blizzard-test.js src/config.js
	@$(PLOVR) build src/config.js > target/blizzard-test.js
	rm src/config.js

deploy: target/sheet-test.js target/snowflake-test.js target/blizzard-test.js
	mkdir -p $(STATIC_DIR)/js
	mkdir -p $(STATIC_DIR)/html
	cp target/*.js $(STATIC_DIR)/js/
	cp html/*.html $(STATIC_DIR)/html/

