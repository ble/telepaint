goog.require('goog.events.EventTarget');
goog.require('goog.events.EventHandler');
goog.require('ble.comet.Queue');
goog.require('ble.json.RpcResponse');
goog.require('ble.json.RpcCall');
goog.require('ble.rpc.EventType');
goog.require('ble.hate');

goog.provide('ble.room.Connection');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
var EventTarget = goog.events.EventTarget;
var Event = goog.events.Event;
var rpcType = ble.rpc.EventType;
var netType = goog.net.EventType;

/**
 * @constructor
 * @extends {EventTarget}
 */
ble.room.Connection = function() {
  EventTarget.call(this);
  this.handler = new goog.events.EventHandler(this);
};
goog.inherits(ble.room.Connection, EventTarget);

var ccp = ble.room.Connection.prototype;

ccp.handleQueueUpdate = function(event)  {
  if(event.type != ble.comet.Queue.Update.EventType) {
    console.error("Unexpected function type");
    return;
  }

  var obj = event.json;
  if(obj['result']['method'] !== 'queue_update') {
    console.error('unexpected method on comet update');
    return;
  }

  var messages = obj['result']['messages'];
  for(var i = 0; i < messages.length; i++) {
    var message = messages[i];
    dispEvent = new Event(rpcType.RESPONSE);
    dispEvent.method = message['method'];
    console.log(dispEvent.method + dispEvent.method + dispEvent.method);
    dispEvent.target = this;
    dispEvent.result = message;
    this.dispatchEvent(dispEvent); 
    console.log([event.type, dispEvent.method].join(" "));
  }
};

ccp.handleNetSuccess = function(event) {
/** @type {goog.net.XhrIo} */
  var xhr = event.target;
  var rpc = xhr.rpc; 
  var obj = xhr.getResponseJson(); 
  if(ble.json.RpcResponse.isResponse(obj) &&
     goog.isDefAndNotNull(obj['result'])) {

    var response = ble.json.RpcResponse.coerce(obj);
    var result = response.result;
    var isPost = xhr.isPost;
    var type = isPost ? rpcType.CALL_SUCCESS : rpcType.RESPONSE;
    dispEvent = new Event(type);
    dispEvent.result = result;
    dispEvent.response = response;
    if(isPost) {
      xhr.rpc.dispatchEvent(dispEvent);
    } else {
      this.dispatchEvent(dispEvent);
    }
  } else if(obj['error']) {
    dispEvent = new Event(rpcType.CALL_ERROR);
    dispEvent.target = rpc;
    dispEvent.error = obj['error'];
    rpc.dispatchEvent(dispEvent);
  } else {
    console.error('unexpected handleRpcCall');
    console.log(event);
    dispEvent = new Event(rpcType.FORMAT_ERROR);
    dispEvent.target = rpc;
    rpc.dispatchEvent(dispEvent);
  } 
  xhr.dispose();
};

ccp.handleEvent = function(event) {
  if(event.type == netType.SUCCESS) {
    this.handleNetSuccess(event);
  } else if(event.type == netType.ERROR) { 
    var xhr = event.target;
    var rpc = xhr.rpc; 

    dispEvent = new Event(rpcType.TRANSPORT_ERROR);
    dispEvent.target = rpc;
    rpc.dispatchEvent(dispEvent);
    xhr.dispose();
  }
};

ccp.handleRpc = function(o) {
  var res = o['result'];
  if(goog.isDefAndNotNull(res)) {
  } else {
    console.error(o);
    console.error(o['error']);
  }
};

ccp.pollCometFrom = function(when) {
  this.stopComet(); 
  this.comet = new ble.comet.Queue(ble.hate.links()['queue'], when);
  this.handler.listen(
    this.comet,
    ble.comet.Queue.Update.EventType,
    this.handleQueueUpdate);
  this.comet.run();
};

ccp.stopComet = function() {
  if(goog.isDefAndNotNull(this.comet)) {
    this.comet.stop();
    this.handler.unlisten(
      this.comet,
      ble.comet.Queue.Update.EventType,
      this.handleQueueUpdate);
    this.comet = null;
  }
};

/**
 * @param {ble.json.RpcCall} call
 */
ccp.postRpc = function(call) {
  var xhr = new goog.net.XhrIo();
  goog.events.listenOnce(
      xhr,
      [goog.net.EventType.ERROR, goog.net.EventType.SUCCESS],
      this);
  var roomUri = ble.hate.links()['room'];
  var headers = {'Content-Type': 'application/json'};
  xhr.rpc = call;
  xhr.isPost = true;
  xhr.send(roomUri, 'POST', JSON.stringify(call), headers);
};

ccp.getRpc = function(uri) {
  var xhr = new goog.net.XhrIo();
  goog.events.listenOnce(
      xhr,
      [goog.net.EventType.ERROR, goog.net.EventType.SUCCESS],
      this);
  xhr.isPost = false;
  xhr.send(uri);
};

ccp.fetchState = function() {
  this.getRpc(ble.hate.links()['room']);
};

var JSON = window.JSON;
ccp.sendSetName = function(who, name) {
   var xhr = new goog.net.XhrIo();
  goog.events.listenOnce(xhr,[goog.net.EventType.ERROR, goog.net.EventType.SUCCESS], this);
  var roomUri = ble.hate.links()['room'];
  var rpc = new ble.json.RpcCall('set_name', {'who': who, 'name': name});
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
