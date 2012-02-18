goog.provide('ble.room.Observer');
goog.provide('ble.room.Model');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
//////////////////////////////////////////////////////////////////////////////// 
/**
 * @constructor
 * @param {?string} name
 * @param {string} id
 */
ble.room.Observer = function(name, id) {
  this.name = name;
  this.id = id;
};

/**
 * @constructor
 * @param {string} name
 * @param {Array.<ble.room.Observer>} observers
 * @param {?ble.room.Observer} obsSelf
 */
ble.room.Model = function(name, observers, obsSelf) {
  this.name = name;
  this.observers = observers;
  this.obsSelf = obsSelf;
};

var mp = ble.room.Model.prototype;
mp.myName = function() {
  return this.obsSelf.name;
};
////////////////////////////////////////////////////////////////////////////////
                                                                             });
//////////////////////////////////////////////////////////////////////////////// 
