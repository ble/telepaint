goog.require('ble.scratch.Canvas');

goog.require('ble.blizzard.Client');
goog.require('ble.blizzard.State');
goog.require('ble.blizzard.EventType');

goog.require('goog.dom.DomHelper');

goog.provide('ble.blizzard.run_test');

ble.blizzard.run_test = function() { 
  var domHelper = new goog.dom.DomHelper();
  var container = domHelper.getElement("outermost");

  var aspect = 0.75;
  var width = 1920;
  var height = aspect * width;
  var canvas = new ble.scratch.Canvas(width, height);
  canvas.render(container);  
  //Figure out the URL to interact with
  var url = "/blizzard";

  //Create the state and the client objects
  var state = new ble.blizzard.State(canvas);
  
  var client = new ble.blizzard.Client(url);

  //Wire the client to the state
  goog.events.listen(
    client,
    [ble.blizzard.EventType.LOADED_BLIZZARD, ble.blizzard.EventType.LOADED_SNOWFLAKE],
    state);

  client.stateRequest().send();
};
