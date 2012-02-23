goog.provide('ble.json.RpcCall');

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
