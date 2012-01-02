#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin \
         -config telepaint.config
         -boot start_sasl \
         -s reloader \
         -s warmup start_app telepaint
