goog.require('goog.events.EventTarget');
goog.require('goog.events');
goog.require('goog.net.Cookies');
goog.require('goog.net.XhrIo');

goog.require('goog.ui.Prompt');

goog.require('ble.json.RpcCall');
goog.require('ble.hate');
goog.require('ble.erlToDate');
goog.require('ble.room.Observer');
goog.require('ble.room.Model');
goog.require('ble.room.Dom');

goog.require('ble.comet.Basic');

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
  DISCONNECTED: 'DISCONNECTED',
  RPC_FAILED: 'RPC_FAILED'
});
var EventType = ble.room.EventType;

/**
 * @constructor
 * @param {ble.room.Dom} dom
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
  goog.events.listen(this.connection, [ble.rpc.EventTypes.RESPONSE, EventType.FETCHED_STATE, EventType.RPC_FAILED], this);
  goog.events.listen(this.dom, [ble.room.Dom.EventType.CHAT], this);
};
goog.inherits(ble.room.Client, goog.events.EventTarget);


var cp = ble.room.Client.prototype;

cp.handleEvent = function(event) {
  console.log('Client.handleEvent called');
  console.log(event);
  switch(event.type) {
    case ble.rpc.EventTypes.RESPONSE:
      this.handleMethod(event.method, event.result);
      break;

    case ble.room.Dom.EventType.CHAT:
      this.connection.sendChat(event.msg);
      break;

    case EventType.RPC_FAILED:
      this.handleFailedRpc(event);
      break;
    default:
      console.log("Unhandled event of type " + event.type);
  }
};

cp.handleFailedRpc = function(event) {
  switch(event.method) {
    case 'set_name':
      this.promptForName('Server says: "' + event.error.message + '"');
      break;
    default:
      console.error('RPC ' + event.method + ' failed.');
  }
};

cp.promptForName = function(message) {
  message = goog.isDefAndNotNull(message) ? message : 'Pick a name that others will see.'; 
  if(!goog.isDefAndNotNull(this.state.myName())) {
     this.dlg = new goog.ui.Prompt(
         'Choose your handle',
         message,
         goog.bind(this.pickName, this),
         'modal-dialog');
     this.dlg.setVisible(true);
  } 
};

cp.updateState = function(state) {
  this.state = state;
  this.dom.set(this.state);
  this.promptForName();
};

cp.handleMethod = function(method, obj) {
  switch(method) {
    case 'set_name':
      var who = obj['who'], name = obj['name'];
      this.state.byId[who].name = name;
      this.dom.set(this.state);
      break;

    case 'room_state':
      var rO = obj['observers'];
      var observers = [];
      var self = null;
      for(var i = 0; i < rO.length; i++) {
        var obs = new Observer(rO[i]['name'], rO[i]['id']);
        if(rO[i]['self'])
          self = obs;
        observers.push(obs);
      }
      var model = new ble.room.Model(obj['name'], observers, self);
      this.updateState(model);
      this.connection.pollCometFrom(obj['when']);
      break;

    case 'join_room':
      var who = obj['who'], name = obj['name'];
      if(!name) name = null;
      var joined = new ble.room.Observer(name, who);
      this.state.addObserver(joined);
      this.updateState(this.state);
      break;

    case 'chat':
      var who = obj['who'], message = obj['message'];
      this.dom.chat(this.state.getObserver(who), message);
      break;

    default:
      console.error('Unknown method: ' + method);
      console.error(obj);
  }
};

cp.pickName = function(nameString) {
  var acceptable = /\s*([a-zA-Z0-9_][!-~]+)\s*/;
  var match = nameString.match(acceptable);
  if(!match) {
    var dlg = this.dlg;
    var showAgain = goog.bind(dlg.setVisible, dlg, true);
    window.setTimeout(showAgain, 0);
    return;
  }
  console.log(match[1]);

  this.connection.sendSetName(
    this.state.obsSelf.id,
    match[1]);
};

cp.setupLinks = function() {
  var roomUri = '/room/' + this.roomId;
  var queueUri = roomUri + '/queue/' + this.observerId;
  var obj = (
      {'room': roomUri,
       'queue': queueUri});
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
  console.log('Connection.handleEvent called');
  console.log(event);
  if(event.type == goog.net.EventType.SUCCESS) {
    console.log(event.target);

    /** @type {goog.net.XhrIo} */
    var xhr = event.target;
    var obj = xhr.getResponseJson(); 
    if(ble.json.RpcResponse.isResponse(obj) && goog.isDefAndNotNull(obj['result'])) {
      this.handleRpc(ble.json.RpcResponse.coerce(obj));
    } else {
      if(xhr.rpc) {
        var method = xhr.rpc['method'];
        var event = new goog.events.Event(EventType.RPC_FAILED);
        event.method = method;
        event.error = obj['error'];
        this.dispatchEvent(event);
      }
      console.error(event);
    }
    goog.events.unlisten(event.target, event.type, this);
  }
  if(event.type == goog.net.EventType.ERROR) {
    console.error(event);
  }
  if(event.type == ble.comet.Queue.Update.EventType) {
    var obj = event.json;
    if(obj['result']['method'] !== 'queue_update')
      throw new Error('unexpected method on comet update');
    var messages = obj['result']['messages'];
    for(var i = 0; i < messages.length; i++) {
      var message = messages[i];
      var event = new goog.events.Event(ble.rpc.EventTypes.RESPONSE);
      event.method = message['method'];
      console.log(event.method + event.method + event.method);
      event.target = this;
      event.result = message;
      this.dispatchEvent(event);
    }
  };
};

ccp.handleRpc = function(o) {
  var res = o['result'];
  if(goog.isDefAndNotNull(res)) {
    var method = res['method'];
    var event = new goog.events.Event(ble.rpc.EventTypes.RESPONSE);
    event.method = method;
    event.target = this;
    event.result = res;
    this.dispatchEvent(event);
  } else {
    console.error(o);
    console.error(o['error']);
  }
};

ccp.pollCometFrom = function(when) {
  this.stopComet(); 
  this.comet = new ble.comet.Queue(ble.hate.links()['queue'], when);
  goog.events.listen(this.comet, this.comet.dispatchedEventTypes, this);
  this.comet.run();
};

ccp.stopComet = function() {
  if(goog.isDefAndNotNull(this.comet)) {
    this.comet.stop();
    goog.events.unlisten(this.comet, this.comet.dispatchedEventTypes, this);
    this.comet = null;
  }
};

ccp.fetchState = function() {
  var xhr = new goog.net.XhrIo();
  goog.events.listen(xhr,[goog.net.EventType.ERROR, goog.net.EventType.SUCCESS], this);
  var roomUri = ble.hate.links()['room'];
  xhr.send(roomUri, 'GET'); 
};

var JSON = window.JSON;
ccp.sendSetName = function(who, name) {
   var xhr = new goog.net.XhrIo();
  goog.events.listen(xhr,[goog.net.EventType.ERROR, goog.net.EventType.SUCCESS], this);
  var roomUri = ble.hate.links()['room'];
  var rpc = new ble.json.RpcCall('set_name', {'who': who, 'name': name});
  xhr.rpc = rpc;
  xhr.send(
    roomUri,
    'POST',
    JSON.stringify(rpc),
    {'Content-Type': 'application/json'}); 
 
};

ccp.sendChat = function(message) {
  var xhr = new goog.net.XhrIo();
  goog.events.listen(xhr,[goog.net.EventType.ERROR, goog.net.EventType.SUCCESS], this);
  var roomUri = ble.hate.links()['room'];
  var rpc = new ble.json.RpcCall('chat', {'message': message});
  xhr.rpc = rpc;
  xhr.send(
    roomUri,
    'POST',
    JSON.stringify(rpc),
    {'Content-Type': 'application/json'}); 
 

};

////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
var errorHandler = new goog.debug.ErrorHandler(function(e) {
  console.log('Intercepted error:');
  console.error(e);
  window.lastError = e;
  throw e;
});
var dom = new ble.room.Dom();
dom.render(document.body);

goog.net.XhrIo.protectEntryPoints(errorHandler);
var client = new ble.room.Client(dom);
client.setupLinks();
client.fetchState();


