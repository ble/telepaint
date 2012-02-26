goog.provide('ble.json.RpcCall');
goog.provide('ble.rpc.EventTypes');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
/**
 * @enum {string}
 */
ble.rpc.EventTypes = ({
  CALL: 'CALL',
  RESPONSE: 'RESPONSE',
  ERROR: 'ERROR'});

var g = goog.isDefAndNotNull;

ble.json.rpcVersion = "2.0";
/**
 * @constructor
 * @param {string} method
 * @param {*} params
 * @param {(string|number)=} id
 */
ble.json.RpcCall = function(method, params, id) {
  this['version'] = ble.json.rpcVersion;
  this['method'] = method;
  this['params'] = params;
  if(goog.isDefAndNotNull(id))
    this['id'] = id;
};

ble.json.RpcCall.coerce = function(obj) {
  var call = new ble.json.RpcCall(obj['method'], obj['params'], obj['id']);
  call['version'] = obj['version'];
  return call;
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
