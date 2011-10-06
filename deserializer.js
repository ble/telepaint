goog.provide('ble.json.TaggedDeserializer');


/**
 * @constructor
 */

ble.json.TaggedDeserializer = function() {
  this.tags = {};
};


ble.json.TaggedDeserializer.prototype.register = function(constructor) {
  var tag = constructor.prototype._tag;
  var bless = constructor.prototype.bless;
  this.tags[tag] = bless;
};

ble.json.TaggedDeserializer.prototype.deserialize = function(o) {
  var tag = o['_tag'];
  if(tag in this.tags) {
    var bless = this.tags[tag];
    return bless(o);
  }
  return null;
};
