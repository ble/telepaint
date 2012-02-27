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
  this.byId = {};
  for(var i = 0; i < observers.length; i++) {
    this.byId[observers[i].id] = observers[i];
  }
};

ble.room.Model.prototype.addObserver = function(observer) {
  var already = this.byId[observer.id];
  if(!already) {
    this.observers.push(observer);
    this.byId[observer.id] = observer;
  } else {
    if(observer.name)
      already.name = observer.name;
  }
};

var mp = ble.room.Model.prototype;
mp.myName = function() {
  return this.obsSelf.name;
};
////////////////////////////////////////////////////////////////////////////////
                                                                             });
//////////////////////////////////////////////////////////////////////////////// 
