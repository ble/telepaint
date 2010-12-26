fs            = require 'fs'
path          = require 'path'
exec          = require('child_process').exec
sources = 'client/src/doodle.coffee'
output = 'priv/www/scripts/'


task 'build', ->
  exec ('coffee -o ' + output + ' -c ' + sources), (err) ->
    console.log err if err
