goog.require('goog.events.EventTarget');

goog.provide('ble.json.RpcCall');
goog.provide('ble.json.RpcResponse');
goog.provide('ble.rpc.EventType');
goog.provide('ble.rpc.id');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
/**
 * @enum {string}
 */
ble.rpc.EventType = ({
  RESPONSE: 'RESPONSE',
  CALL_SUCCESS: 'CALL_SUCCESS',
  CALL_ERROR: 'ERROR',
  TRANSPORT_ERROR: 'TRANSPORT_ERROR',
  FORMAT_ERROR: 'FORMAT_ERROR'});

ble.rpc.EventType.ALL = goog.object.getValues(ble.rpc.EventType);

ble.rpc.id = function() {
  var scale = 1 << 30;
  var ts = Date.now();
  var random = Math.floor(scale * Math.random());
  return ts ^ random;
};
var g = goog.isDefAndNotNull;

ble.json.rpcVersion = "2.0";
/**
 * @constructor
 * @param {string} method
 * @param {*} params
 * @param {(string|number)=} id
 * @extends {goog.events.EventTarget}
 */
ble.json.RpcCall = function(method, params, id) {
  goog.events.EventTarget.call(this);
  this.version = ble.json.rpcVersion;
  this.method = method;
  this.params = params;
  if(goog.isDefAndNotNull(id))
    this.id = id;
};
goog.inherits(ble.json.RpcCall, goog.events.EventTarget);

ble.json.RpcCall.coerce = function(obj) {
  var call = new ble.json.RpcCall(obj['method'], obj['params'], obj['id']);
  call.version = obj['version'];
  return call;
};

ble.json.RpcCall.prototype.toJSON = function() {
  var obj = {
    'version': this.version,
    'method': this.method,
    'params': this.params };
  if(goog.isDefAndNotNull(this.id))
    obj['id'] = this.id;
  return obj;
};

/**
 * @constructor
 * @param {?(Object|Array)} result
 * @param {Object=} error
 * @param {(number | string)=} id
 */
ble.json.RpcResponse = function(result, error, id) {
  if(goog.isDefAndNotNull(result) && goog.isDefAndNotNull(error))
    throw new Error('rpc response must have exactly one of result and error.');
  this['version'] = ble.json.rpcVersion;
  if(g(result))
    this['result'] = result;
  if(g(error))
    this['error'] = error;
  if(g(id))
    this['id'] = id;
};

ble.json.RpcResponse.isResponse = function(obj) {
  return ( g(obj['result'] || obj['error']) && g(obj['version']));
};

ble.json.RpcResponse.coerce = function(o) {
  var response = new ble.json.RpcResponse(o['result'], o['error'], o['id']);
  response['version'] = o['version'];
  return response;
};

////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
