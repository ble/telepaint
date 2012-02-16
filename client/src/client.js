goog.require('goog.events.EventTarget');
goog.require('goog.events');
goog.require('goog.net.Cookies');
goog.require('goog.net.XhrIo');

goog.require('ble.hate');
goog.require('ble.erlToDate');
goog.require('ble.room.Observer');
goog.require('ble.room.Model');
goog.require('ble.room.Dom');

goog.require('goog.debug.ErrorHandler');

goog.provide('ble.room.Client');
var console = window.console;

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////

var Observer = ble.room.Observer;
var Model = ble.room.Model;
/**
 * @enum{string}
 */
ble.room.EventType = ({
  FETCHED_STATE: 'FETCHED_STATE',
  UPDATE: 'UPDATE',
  DISCONNECTED: 'DISCONNECTED'
});
var eventType = ble.room.EventType;

/**
 * @constructor
 * @param {ble.room.Dom}
 * @extends {goog.events.EventTarget}
 */ 
ble.room.Client = function(dom) {
  goog.events.EventTarget.call(this);
  this.dom = dom;
  var pattern = /^http:\/\/([^\/]*)\/room\/([0-9a-zA-Z_-]+)\/client(?:\?join)?$/;
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
  goog.events.listen(this.connection, eventType.FETCHED_STATE, this);
};
goog.inherits(ble.room.Client, goog.events.EventTarget);


var cp = ble.room.Client.prototype;

cp.handleEvent = function(event) {
  console.log('Client.handleEvent called');
  console.log(event);
  if(event.type == eventType.FETCHED_STATE) {
    dom.set(event.room);
  }
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
    this.handleResponse(obj);
    goog.events.unlisten(event.target, event.type, this);
  }
  if(event.type == goog.net.EventType.ERROR) {
    console.error(event);
  }
};

ccp.handleResponse = function(respObj) {
  var type = respObj['type'];
  if(type == 'room') {
    var observers = [];
    var self = null;

    var respObservers = respObj['observers'];
    for(var i = 0; i < respObservers.length; i++) {
      var respObs = respObservers[i];
      var o = new Observer(respObs['name'], respObs['id']);
      observers.push(o);
      if(respObs.self)
        self = o;
    };
    var fetchEvent = new goog.events.Event(eventType.FETCHED_STATE);
    fetchEvent.room = new Model(respObj.name, observers, self);
    this.dispatchEvent(fetchEvent);
  } else {
    console.error(['don\'t know how to handle response:', respObj]);
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
var dom = new ble.room.Dom();
dom.render(document.body);

goog.net.XhrIo.protectEntryPoints(errorHandler);
var client = new ble.room.Client(dom);
client.setupLinks();
client.fetchState();


