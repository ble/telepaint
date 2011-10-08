
goog.require('goog.json.Serializer');
goog.provide('ble.json.PrettyPrinter');

/**
 * @constructor
 * @param {string=} opt_tab
 * @extends {goog.json.Serializer}
 */ 
ble.json.PrettyPrinter = function(opt_tab) {
  goog.json.Serializer.call(this);
  this.tab = goog.isDef(opt_tab) ? opt_tab : "\t";
  this.indentLevel = 0;
};
goog.inherits(ble.json.PrettyPrinter, goog.json.Serializer);

ble.json.PrettyPrinter.prototype.currentIndent_ = function() {
  return goog.string.repeat(this.tab, this.indentLevel);
};

ble.json.PrettyPrinter.prototype.serializeObject_ = function(obj, sb) {
  if(goog.isDef(obj.toJSON))
    obj = obj.toJSON();
  this.indentLevel++;
  sb.push('{');
  var sep = "\n";
  for(var key in obj) {
    if(Object.prototype.hasOwnProperty.call(obj, key)) {
      var value = obj[key];
      if(typeof value == 'function')
        continue;
      sb.push(sep);
      sb.push(this.currentIndent_());
      this.serializeString_(key, sb);
      sb.push(":");
      this.serialize_(value, sb);
      sep = ",\n";
    }
  }
  this.indentLevel--;
  sb.push('\n');
  sb.push(this.currentIndent_());
  sb.push('}');
};


