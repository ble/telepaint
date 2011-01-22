fs            = require 'fs'
path          = require 'path'
exec          = require('child_process').exec
sources = 'client/src/*.coffee'
extern = 'client/extern/*.js'
output = 'priv/www/scripts/'
testOutput = 'client/test/scripts'


task 'build', ->
  exec ('coffee -o ' + output + ' -c ' + sources), (err) ->
    console.log err if err
  exec ('cp ' + extern + ' ' + output), (err) ->
    console.log err if err

task 'test', ->
  exec ('coffee -o ' + testOutput + ' -c ' + sources), (err) ->
    console.log err if err
  exec ('cp ' + extern + ' ' + testOutput), (err) ->
    console.log err if err
