goog.require('goog.events.EventTarget');
goog.require('goog.events.Event');
goog.require('goog.net.XhrIo');
goog.require('goog.structs.LinkedMap');

goog.provide('ble.sheet.Client');
goog.provide('ble.sheet.EventType');

/**
 * @enum {string}
 */
ble.sheet.EventType = {
  UPDATE: 'update',
  FETCH: 'fetch'
};

/**
 * @constructor
 * @extends{goog.events.EventTarget}
 */
ble.sheet.Client = function(url) {
  goog.events.EventTarget.call(this);
  this.url = url;
  this.fragments = new goog.structs.LinkedMap();
}

ble.sheet.Client.prototype.headers = {};
ble.sheet.Client.prototype.headers[goog.net.XhrIo.CONTENT_TYPE_HEADER] = "application/json";

goog.inherits(ble.sheet.Client, goog.events.EventTarget);

var JSON;
var console;

ble.sheet.Client.prototype.rpc_ = function(method, data) {
  var xhrIo = new goog.net.XhrIo();
  var rpc = {'method': method, 'data': data, 'clientTime': Date.now()};
  var send = goog.bind(xhrIo.send, xhrIo, this.url, 'POST', JSON.stringify(rpc));
  xhrIo.send = send;
  xhrIo.rpc = rpc;
  return xhrIo;
};

ble.sheet.Client.prototype.append = function(appendType, data) {
  var xhrIo = this.rpc_(appendType, data);
  var client = this;
  goog.events.listenOnce(xhrIo, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      client.insert_(this.rpc);
      client.dispatchEvent(new goog.events.Event(ble.sheet.EventType.UPDATE));
    } else {
      alert('error on Client.append');
    }
  });
  return xhrIo;
};

ble.sheet.Client.prototype.undo = function() {
  var toUndo = this.fragments.head_.prev.key;
  var xhrIo = this.rpc_('undo', {'clientTimeToUndo': toUndo});
  var client = this;
  goog.events.listenOnce(xhrIo, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      client.fragments.remove(toUndo);
      client.dispatchEvent(new goog.events.Event(ble.sheet.EventType.UPDATE));
    } else {
      alert('error on Client.undo');
    }
  });
  return xhrIo;
};

ble.sheet.Client.prototype.insertAll_ = function(fragments) {
  for(var i = 0; i < fragments.length; i++) {
    this.insert_(fragments[i]);
  }
};

ble.sheet.Client.prototype.insert_ = function(fragment) {
  var clientTime = fragment.clientTime;
  if(!goog.isDef(clientTime) && goog.isDef(fragment.data))
    clientTime = fragment.data.startTime;
  if(!goog.isDef(clientTime))
    return;
  this.fragments.set(clientTime, fragment);
};

ble.sheet.Client.prototype.read = function() {
  var xhrIo = new goog.net.XhrIo();
  var send = goog.bind(xhrIo.send, xhrIo, this.url, 'GET');
  xhrIo.send = send;

  var client = this;
  goog.events.listenOnce(xhrIo, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      var json = this.getResponseJson();
      client.insertAll_(json.fragments);
      var event = new goog.events.Event(ble.sheet.EventType.FETCH);
      client.dispatchEvent(event);
    } else {
      alert('error on Client.read');
    }
  });
  return xhrIo;
};
