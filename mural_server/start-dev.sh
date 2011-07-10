#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname mural_server_dev \
    -s mural_server \
    -s reloader \
    -mnesia dir "./.mnesia"
