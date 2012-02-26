SERVE=java -jar plovr.jar serve
BUILD=java -jar plovr.jar build

SRCS=room-client.json comet-test.json
COMPILATIONS=${SRCS:.json=.js} 

.SUFFIXES: .js .json

.json.js :
	@$(BUILD) $< > $*.js

all : $(COMPILATIONS)
	echo done

clean :
	rm *.js $(OUTDIR)/*.js

serve :
	@$(SERVE) $(SRCS)
