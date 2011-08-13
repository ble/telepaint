goog.require('goog.events.Event');
goog.require('goog.events.EventTarget');

goog.require('goog.net.XhrIo');

goog.require('goog.structs.LinkedMap');

goog.provide('ble.snowflake.Painter');
goog.provide('ble.snowflake.Client');
goog.provide('ble.snowflake.EventType');

goog.require('ble.gfx');

var JSON;

/**
 * @param {ble.scratch.DrawSurface} surface
 * @constructor
 * @implements {ble.scratch.Drawable}
 */

ble.snowflake.Painter = function(surface) {
  this.surface = surface;
  this.currentInteraction = null;
};

/**
 * @override
 */
ble.snowflake.Painter.prototype.drawTo = function(ctx) {
  ctx.fillStyle = "#fff";
  ctx.beginPath();
  ctx.arc(0, 0, 1, 0, 2 * Math.PI);
  ctx.fill();
  if(this.currentInteraction != null) 
    this.drawFragment(ctx, this.currentInteraction); 
};

/**
 * @param {ble.snowflake.Client} client
 * @param {goog.events.Event} event
 */
ble.snowflake.Painter.prototype.initClient = function(client, event) {

};

/**
 * @param {ble.snowflake.Client} client
 * @param {goog.events.Event} event
 */
ble.snowflake.Painter.prototype.updateClient = function(client, event) {

};

/**
 * @param {Object} interaction
 */
ble.snowflake.Painter.prototype.setCurrentInteraction = function(interaction) {
  this.currentInteraction = interaction;
};

ble.snowflake.Painter.prototype.repaint = function() {
  this.surface.withContext(goog.bind(this.drawTo, this));
};

ble.snowflake.Painter.prototype.drawFragment = function(ctx, fragment) {
  if(fragment.method == "erase-polyline") {
    var coordinates = fragment.getControlCoordinatesAndHead();
    if(!goog.isDef(coordinates))
      return; 
    console.log(coordinates);
    ctx.fillStyle = "rgba(0, 0, 0, 0.0)";
    ctx.strokeStyle = "rgba(0, 0, 0, 1.0)";
    ctx.lineWidth *= 3.0;
    ctx.globalCompositeOperation = "copy";
    if(coordinates.length == 0)
      return;
    for(var flip = 0; flip < 2; flip++) {
      for(var rot = 0; rot < 6; rot++) {
        ble.gfx.pathCoords(ctx, coordinates);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
        ctx.rotate(Math.PI / 3.0);
      }
      ctx.scale(-1, 1);
    }
  }
};

/**
 * Constants for client event names
 * @enum {string}
 */
ble.snowflake.EventType = {
  INIT: 'init',
  UPDATE: 'update'
};

/**
 * @param {string} url
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.snowflake.Client = function(url) {
  goog.events.EventTarget.call(this);
  this.url = url;
  this.allFragments = new goog.structs.LinkedMap();
  this.visibleFragments = new goog.structs.LinkedMap();
};
goog.inherits(ble.snowflake.Client, goog.events.EventTarget);

/**
 * @param {string} method
 * @param {Object} data
 */
ble.snowflake.Client.prototype.rpc_ = function(method, data) {
  var xhr = new goog.net.XhrIo();
  var rpc = {'method': method, 'data': data, 'clientTime': Date.now()};
  var send = goog.bind(xhr.send, xhr, this.url, 'POST', JSON.stringify(rpc));
  xhr.send = send;
  xhr.rpc = rpc;
  return xhr;
};

ble.snowflake.Client.prototype.rpcAppend = function(method, data) {
  var xhr = this.rpc_(method, data);
  var client = this;
  goog.events.listenOnce(xhr, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      client.append_(this.rpc);
      var event = new goog.events.Event(ble.snowflake.EventType.UPDATE);
      event.fragment = this.rpc;
      client.dispatchEvent(event);
    } else {
      alert('error on Client.rpcAppend');
    }
  });
  return xhr;
};

ble.snowflake.Client.prototype.rpcUndo = function() {
  var toUndo = this.visibleFragments.head_.prev.key;
  var xhr = this.rpc_('undo', {'clientTimeToUndo': toUndo});
  var client = this;
  goog.events.listenOnce(xhr, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      client.append_(this.rpc);
      client.remove_(toUndo);
      var event = new goog.events.Event(ble.snowflake.EventType.UPDATE);
      event.fragment = this.rpc;
      client.dispatchEvent(event);
    } else {
      alert('error on Client.rpcUndo');
    }
  });
  return xhr;
};

ble.snowflake.Client.prototype.readState = function() {
  var xhr = new goog.net.XhrIo();
  var send = goog.bind(xhr.send, xhr, this.url, 'GET');
  xhr.send = send;
  var client = this;
  goog.events.listenOnce(xhr, goog.net.EventType.COMPLETE,  function(e) {
    if(this.isSuccess()) {
      var json = this.getResponseJson();

      if(!goog.isDef(json.fragments))
        return;

      client.clear_();
      client.insertAll_(json.fragments);
      client.dispatchEvent(new goog.events.Event(ble.snowflake.EventType.INIT));
    } else {
      alert('error on Client.readState');
    } 
  });
}

ble.snowflake.Client.prototype.append_ = function(fragment) {
  if(fragment.method != "undo")
    this.visibleFragments.set(fragment.clientTime, fragment);
  this.allFragments.set(fragment.clientTime, fragment); 
};

ble.snowflake.Client.prototype.remove_ = function(clientTime) {
  this.visibleFragments.remove(clientTime); 
};

ble.snowflake.Client.prototype.clear_ = function() {
  this.allFragments.clear();
  this.visibleFragments.clear();
}

ble.snowflake.Client.prototype.insertAll_ = function(fragments) {
  if(fragments.length == 0)
    return;

  for(var i = 0, fragment = fragments[i];
      i < fragments.length;
      i++, fragment = fragments[i]) {
    this.append_(fragment);
    if(fragment.method == "undo") {
      this.remove_(fragment.data.clientTimeToUndo);
    }
  } 
  this.dispatchEvent(new goog.events.Event(ble.snowflake.EventType.INIT)); 
};
