goog.require('goog.events.EventTarget');
goog.require('goog.events.Event');
goog.require('goog.net.XhrIo');

goog.provide('ble.sheet.Client');
goog.provide('ble.sheet.EventType');

/**
 * @enum {string}
 */
ble.sheet.EventType = {
  SUCCESS: 'success',
  FETCH: 'fetch'
};

/**
 * @constructor
 * @extends{goog.events.EventTarget}
 */
ble.sheet.Client = function(url) {
  goog.events.EventTarget.call(this);
  this.url = url;
}

goog.inherits(ble.sheet.Client, goog.events.EventTarget);

var JSON;
var console;

ble.sheet.Client.prototype.sheetAppend = function(method, data) {
  var xhrIo = new goog.net.XhrIo();
  var rpc = {'method': method, 'data': data};
  var send = goog.bind(xhrIo.send, xhrIo, this.url, 'POST', JSON.stringify(rpc));
  xhrIo.send = send;
  return xhrIo;
}

ble.sheet.Client.prototype.read = function() {
  var xhrIo = new goog.net.XhrIo();
  var send = goog.bind(xhrIo.send, xhrIo, this.url, 'GET');
  xhrIo.send = send;

  var client = this;
  goog.events.listenOnce(xhrIo, goog.net.EventType.COMPLETE, function(e) {
    if(this.isSuccess()) {
      console.log(this.getResponse());
      var json = this.getResponseJson();
      var e = new goog.events.Event(ble.sheet.EventType.FETCH);
      e.json = json;
      client.dispatchEvent(e);
    } else {
      alert('error on Client.read');
    }
    this.dispose();
  });
  return xhrIo;
};
