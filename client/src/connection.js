goog.require('goog.events.EventTarget');
goog.require('ble.comet.Queue');
goog.require('ble.json.RpcResponse');
goog.require('ble.json.RpcCall');
goog.require('ble.rpc.EventTypes');
goog.require('ble.hate');

goog.provide('ble.room.Connection');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
var EventTarget = goog.events.EventTarget;
var Event = goog.events.Event;
var rpcTypes = ble.rpc.EventTypes;

/**
 * @constructor
 * @extends {EventTarget}
 */
ble.room.Connection = function() {
  EventTarget.call(this);
};
goog.inherits(ble.room.Connection, EventTarget);

var ccp = ble.room.Connection.prototype;
ccp.handleEvent = function(event) {
  var dispEvent;
  console.log('Connection.handleEvent called');
  console.log(event);

  if(event.type == ble.comet.Queue.Update.EventType) {
    var obj = event.json;
    if(obj['result']['method'] !== 'queue_update')
      throw new Error('unexpected method on comet update');
    var messages = obj['result']['messages'];
    for(var i = 0; i < messages.length; i++) {
      var message = messages[i];
      dispEvent = new Event(rpcTypes.RESPONSE);
      dispEvent.method = message['method'];
      console.log(dispEvent.method + dispEvent.method + dispEvent.method);
      dispEvent.target = this;
      dispEvent.result = message;
      this.dispatchEvent(dispEvent);
    }
  } else if(
      event.type == goog.net.EventType.SUCCESS ||
      event.type == goog.net.EventType.ERROR) {
    /** @type {goog.net.XhrIo} */
    var xhr = event.target;
    var rpc = xhr.rpc;
    if(event.type == goog.net.EventType.SUCCESS) {
      var obj = xhr.getResponseJson(); 

      //successful RPC
      if(ble.json.RpcResponse.isResponse(obj) &&
         goog.isDefAndNotNull(obj['result'])) {

        var response = ble.json.RpcResponse.coerce(obj);
        var result = response.result;
        var isPost = xhr.isPost;
        var type = isPost ? rpcTypes.CALL_SUCCESS : rpcTypes.RESPONSE;
        dispEvent = new Event(type);
        dispEvent.result = result;
        dispEvent.response = response;
        if(isPost) {
          xhr.rpc.dispatchEvent(dispEvent);
        } else {
          this.dispatchEvent(dispEvent);
        }
      } else if(obj['error']) {
        dispEvent = new Event(rpcTypes.CALL_ERROR);
        dispEvent.target = rpc;
        dispEvent.error = obj['error'];
        rpc.dispatchEvent(dispEvent);
      } else {
        console.error('unexpected handleRpcCall');
        console.log(event);
        dispEvent = new Event(rpcTypes.FORMAT_ERROR);
        dispEvent.target = rpc;
        rpc.dispatchEvent(dispEvent);
      }
    }

    if(event.type == goog.net.EventType.ERROR) {
      dispEvent = new Event(rpcTypes.TRANSPORT_ERROR);
      dispEvent.target = rpc;
      rpc.dispatchEvent(dispEvent);
      console.error(event);
    }
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
