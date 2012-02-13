goog.require('goog.events.EventTarget');
goog.require('goog.events');
goog.require('goog.net.Cookies');
goog.require('goog.net.XhrIo');
goog.require('ble.hate');
goog.require('ble.erlToDate');

goog.require('goog.debug.ErrorHandler');

goog.provide('ble.room.Client');
var console = window.console;

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
/**
 * @constructor
 * @extends {goog.events.EventTarget}
 */ 
ble.room.Client = function() {
  goog.events.EventTarget.call(this);
  var pattern = /^http:\/\/([^\/]*)\/room_client\/([0-9a-zA-Z_-]+)(?:\?join)?$/;
  var match = window.location.href.match(pattern);
  if(!match)
    throw new Error('unexpected location');
  this.host = match[1];
  this.roomId = match[2];
  var c = new goog.net.Cookies(document);
  this.observerId = c.get('observerId');
  this.state = null;
  this.lastUpdated = 0;
  this.connection = new ble.room.Client.Connection();
  goog.events.listen(this.connection, 'FETCHED', this);
};
goog.inherits(ble.room.Client, goog.events.EventTarget);


var cp = ble.room.Client.prototype;

cp.handleEvent = function(event) {
  console.log("Client.handleEvent called");
  console.log(event);
};

cp.setupLinks = function() {
  var obj = (
      {'room': '/room/' + this.roomId,
       'queue': '/queue/' + this.observerId});
  ble.hate.addLinks(obj);
};

cp.fetchState = function() {
  this.connection.fetchState();
};

/**
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.room.Client.Connection = function() {
  goog.events.EventTarget.call(this);
};
goog.inherits(ble.room.Client.Connection, goog.events.EventTarget);

var ccp = ble.room.Client.Connection.prototype;
ccp.handleEvent = function(event) {
  console.log("Connection.handleEvent called");
  if(event.type == goog.net.EventType.SUCCESS) {
    console.log(event.target);

    /** @type {goog.net.XhrIo} */
    var xhr = event.target;
    var obj = xhr.getResponseJson(); 
    var evt = new goog.events.Event("FETCHED");
    if('when' in obj)
      obj['when'] = ble.erlToDate(obj['when']);
    evt.data = obj;
    this.dispatchEvent(evt);

    goog.events.unlisten(event.target, event.type, this);
  }
  if(event.type == goog.net.EventType.ERROR) {
    console.error(event);
  }
};

ccp.fetchState = function() {
  var xhr = new goog.net.XhrIo();
  goog.events.listen(xhr,[goog.net.EventType.ERROR, goog.net.EventType.SUCCESS], this);
  var roomUri = ble.hate.links()['room'];
  xhr.send(roomUri, 'GET'); 
};
////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
var errorHandler = new goog.debug.ErrorHandler(function(e) {
  console.log("Intercepted error:");
  console.error(e);
  window.lastError = e;
});
goog.net.XhrIo.protectEntryPoints(errorHandler);
var client = new ble.room.Client();
client.setupLinks();
client.fetchState();


