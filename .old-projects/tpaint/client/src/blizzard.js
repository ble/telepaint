goog.require('goog.math.Box');
goog.require('goog.net.XhrIo');

goog.require('ble.hexspiral');
goog.require('ble.snowflake.Painter');

goog.provide('ble.blizzard.EventType');
goog.provide('ble.blizzard.State');
goog.provide('ble.blizzard.Client');

var console;
/**
 * @enum{string}
 */
ble.blizzard.EventType = {
  LOADED_BLIZZARD: 'loaded_blizzard',
  LOADED_SNOWFLAKE: 'loaded_snowflake'
};

/**
 * @constructor
 */
ble.blizzard.Painter = function() {
  this.snowflakePainter = new ble.snowflake.Painter();
};

ble.blizzard.Painter.prototype.draw = function(state) {

  var flakes = state.flakes;
  var painter = this.snowflakePainter;

  for(var i = 0; i < flakes.length; i++) {
    var flake = flakes[i];
    flake.subcanvas.withContext(
      goog.bind(function(ctx) {
        painter.drawTo(flake.state, ctx);
      }),
      painter);
  } 
};

/**
 * @constructor
 */
ble.blizzard.State = function(canvas) {
  this.canvas = canvas;
  this.painter = new ble.blizzard.Painter();
  this.flakes = [];
  this.subcanvasSpacing = 150;
  this.subcanvasSize = 140;
  this.centerX = canvas.width_px / 2;
  this.centerY = canvas.height_px / 2;
};

ble.blizzard.State.prototype.handleEvent = function(event) {
  if(event.type == ble.blizzard.EventType.LOADED_BLIZZARD) {
    this.loadedBlizzard(event);
  } else if(event.type == ble.blizzard.EventType.LOADED_SNOWFLAKE) {
    this.loadedSnowflake(event);
  }
};

ble.blizzard.State.prototype.loadedBlizzard = function(event) {
  var flakeUrls = event.json.flakeUrls;
  var client = event.client;
  for(var i = 0; i < flakeUrls.length; i++) {
    client.snowflakeRequest(flakeUrls[i]).send();
  }
};

ble.blizzard.State.prototype.loadedSnowflake = function(event) {
  var state = event.snowflakeState;
  var subcanvas = this.newSubcanvas_();
  this.flakes.push({'subcanvas': subcanvas, 'state': state});
  this.repaint();
};

ble.blizzard.State.prototype.repaint = function() {
  this.painter.draw(this);
}

ble.blizzard.State.prototype.newSubcanvas_ = function() {
  var ix = this.flakes.length;
  var center = ble.hexspiral.location(ix);
  center.scale(this.subcanvasSpacing);
  center.x = Math.round(center.x) + this.centerX;
  center.y = Math.round(center.y) + this.centerY;
  var size = this.subcanvasSize;
  var pixelBox = new goog.math.Box(
    center.y - size / 2,
    center.x + size / 2,
    center.y + size / 2,
    center.x - size / 2);
  var vBox = new goog.math.Box(1, 1, -1, -1);
  return new ble.scratch.Subcanvas(this.canvas, pixelBox, vBox);
};

/**
 * @constructor
 * @extends{goog.events.EventTarget}
 */
ble.blizzard.Client = function(url) {
  goog.base(this);
  this.url = url;
};
goog.inherits(ble.blizzard.Client, goog.events.EventTarget);

ble.blizzard.Client.prototype.stateRequest = function() {
  var xhr = new goog.net.XhrIo();
  var send = goog.bind(xhr.send, xhr, this.url, 'GET');
  xhr.send = send;
  var client = this;
  goog.events.listenOnce(xhr, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      var json = this.getResponseJson();
      if(!goog.isDef(json.flakeUrls))
       return; 
      var event = new goog.events.Event(ble.blizzard.EventType.LOADED_BLIZZARD);
      event.json = json;
      event.json.flakeUrls = json.flakeUrls;
      event.client = client;
      client.dispatchEvent(event); 
      //pain point: mistakenly had the following line for a while
      //this.dispatchEvent(event); 
      console.log(event);
    }

  });
  return xhr;
};

ble.blizzard.Client.prototype.snowflakeRequest = function(snowflakeId) {
  var xhr = new goog.net.XhrIo();
  var send = goog.bind(xhr.send, xhr, "/snowflake/" + snowflakeId, 'GET');
  xhr.send = send;
  var client = this;
  goog.events.listenOnce(xhr, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      var json = this.getResponseJson();
      if(!goog.isDef(json.fragments))
        return;
      var fragments = json.fragments;
      var event = new goog.events.Event(ble.blizzard.EventType.LOADED_SNOWFLAKE);
      var flake = new ble.snowflake.State(null);
      for(var i = 0; i < fragments.length; i++) {
        if(goog.isDef(fragments[i].data.coordinates))
          flake.appendFragment(fragments[i]);
      }
      event.snowflakeState = flake;
      client.dispatchEvent(event);
      //pain point: mistakenly had the following line for a while
      //this.dispatchEvent(event); 

    }
  });
  return xhr;
};
