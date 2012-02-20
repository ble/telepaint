goog.provide('ble.json.RpcCall');

ble.json.rpcVersion = "2.0";

ble.json.RpcCall = function(method, params, id) {
  this['version'] = ble.json.rpcVersion;
  this['method'] = method;
  this['params'] = params;
  if(goog.isDefAndNotNull(id))
    this['id'] = id;
};
