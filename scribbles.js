goog.provide('ble.Scribbles');
goog.provide('ble.scribbleDeserializer');

goog.require('ble.json.TaggedDeserializer');
goog.require('ble.json.PrettyPrinter');
goog.require('ble.gfx.StrokeReplay');
goog.require('ble.gfx.PolylineReplay');

goog.require('goog.storage.mechanism.HTML5LocalStorage'); 
goog.require('goog.events.EventTarget');
goog.require('goog.events.Event');

var JSON = window.JSON;
var console = window.console;
/**
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.Scribbles = function() {
  goog.events.EventTarget.call(this);
  this.storage = new goog.storage.mechanism.HTML5LocalStorage();
  this.deserializer = new ble.json.TaggedDeserializer();
  this.deserializer.register(ble.gfx.StrokeReplay);
  this.deserializer.register(ble.gfx.PolylineReplay); 
  this.initialize_();
};
goog.inherits(ble.Scribbles, goog.events.EventTarget);

ble.scribbleDeserializer = new ble.json.TaggedDeserializer();
ble.scribbleDeserializer.register(ble.gfx.StrokeReplay);
ble.scribbleDeserializer.register(ble.gfx.PolylineReplay); 
ble.scribbleDeserializer.register(ble.gfx.EraseReplay); 

goog.exportSymbol('ble.scribbleDeserializer', ble.scribbleDeserializer);

/**
 * @enum{string}
 */
ble.Scribbles.EventType = ({
  UPDATE: 'update'
});

/**
 * @private
 */
ble.Scribbles.prototype.update_ = function() {
  this.dispatchEvent(new goog.events.Event(ble.Scribbles.EventType.UPDATE));
};

/**
 * @private
 */
ble.Scribbles.prototype.initialize_ = function() {
  var keys = this.read_(this.indexKey_);  
  if(goog.isNull(keys) || !goog.isDef(keys)) {
    keys = [];
    this.write_(this.indexKey_, keys); 
  }
  this.keys = [];
  this.data = {};
  for(var i = 0; i < keys.length; i++) {
    var key = keys[i];
    var scribble = this.read_(key);
    if(!goog.isDefAndNotNull(scribble))
      continue;
    var blessed = this.blessScribble(scribble);
    if(!goog.isDefAndNotNull(blessed))
      continue;
    this.data[key] = blessed;
    this.keys.unshift(key);
  }
  if(this.keys.length > 0) {
    this.currentKey = this.keys[0];
    this.update_();
  } else {
    this.currentKey = null;
  }
};

ble.Scribbles.prototype.blessScribble = function(s) {
  if(!goog.isArray(s))
    return null;
  var result = [];
  for(var i = 0; i < s.length; i++) {
    var asReplay = this.deserializer.deserialize(s[i]);
    if(goog.isNull(asReplay))
      console.log("bless error");
    else
      result.push(asReplay);
  }
  return result;
};

ble.Scribbles.prototype.save = function(scribble) {
  if(!this.currentKey) {
    this.currentKey = Date.now();
    this.keys.unshift(this.currentKey);
    this.update_();
  }
  this.data[this.currentKey] = scribble;
  this.write_(this.currentKey, this.data[this.currentKey]);
  this.write_(this.indexKey_, this.keys)
};

ble.Scribbles.prototype.makeNew = function() {
  this.currentKey = Date.now();
  this.keys.unshift(this.currentKey);
  this.data[this.currentKey] = [];
  this.write_(this.currentKey, this.data[this.currentKey]);
  this.write_(this.indexKey_, this.keys);
  this.update_();
};

ble.Scribbles.prototype.pickle = function() {
  return (new ble.json.PrettyPrinter("  ")).serialize(this.data);
};

/**
 * @private
 * @const
 */
ble.Scribbles.prototype.indexKey_ = "times";

/**
 * @private
 */ 
ble.Scribbles.prototype.read_ = function(key) {
  try {
    var value = JSON.parse(this.storage.get(key));
    return value;
  } catch(error) { 
    console.log("read error");
    console.log(error);
    return undefined;
  } 
};

/**
 * @private
 */
ble.Scribbles.prototype.write_ = function(key, value) {
  try {  
    this.storage.set(key, JSON.stringify(value));
    return true;
  } catch(error) {
    console.log("write error");
    console.log(error);
    return false;
  }
};

