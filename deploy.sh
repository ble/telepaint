#!/bin/sh
rm -rf deploy.tar
rebar clean
rebar compile
cake build
mkdir deploy
cp -R ebin deps/*/ebin priv start-dev.sh deploy
tar -czf deploy.tar deploy
rm -rf deploy

