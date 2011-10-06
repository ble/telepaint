goog.provide('ble.use_canvas_3');

goog.require('ble.Scribble');
goog.require('goog.events');
goog.require('goog.events.EventTarget');
goog.require('goog.ui.Component.EventType');

goog.require('goog.ui.Menu');
goog.require('goog.ui.MenuItem');

goog.require('goog.storage.mechanism.HTML5LocalStorage'); 
goog.require('goog.json.Serializer');

goog.require('ble.json.TaggedDeserializer');
goog.provide('ble.Scribbles');

goog.provide('ble.json.PrettyPrinter');


var console = window.console;
var JSON = window.JSON;

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

/**
 * @constructor
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

ble.use_canvas_3 = function() {
  var pxWidth = 640;
  var pxHeight = 480;

  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var canvas = new ble.Scribble(pxWidth, pxHeight);

  var scribbles = new ble.Scribbles();
  var drawCurrent = function() {
    if(scribbles.currentKey != null) {
      canvas.painter = new ble.scribble.Painter(scribbles.data[scribbles.currentKey]);
      canvas.withContext(canvas.repaintComplete);
    } 
  };


  var menu = new goog.ui.Menu();
  var stroke = new goog.ui.MenuItem('Stroke');
  var polyline = new goog.ui.MenuItem('Polyline');
  var makeNew = new goog.ui.MenuItem('New');
  var replay = new goog.ui.MenuItem('Replay');
  var save = new goog.ui.MenuItem('Save');
  var json = new goog.ui.MenuItem('Display JSON');

  menu.addChild(stroke, true);
  menu.addChild(polyline, true);
  menu.addChild(new goog.ui.MenuSeparator(), true);
  menu.addChild(save, true);
  menu.addChild(makeNew, true);
  menu.addChild(replay, true);
  menu.addChild(json, true);
  menu.addChild(new goog.ui.MenuSeparator(), true);

  menu.render(container);
  menu.getElement().style["position"] = "relative";
  menu.getElement().style["display"] = "inline-block";

  canvas.render(container);
  canvas.getElement().style["border"] = "1px solid black";


/*
  for(var i = 0; i < scribbles.keys.length; i++) {
    var key = scribbles.keys[i];
    scribbles.currentKey = key;
    scribbles.save(scribbles.data[key]);
  }*/

  if(scribbles.keys.length > 0)
    scribbles.currentKey = scribbles.keys[0];

  drawCurrent();

  var scribbleMenuItems = [];
  var updateMenu = function() {
    for(var i = 0; i < scribbleMenuItems.length; i++) {
      menu.removeChild(scribbleMenuItems[i], true);
    }
    scribbleMenuItems = [];
    for(var i = 0; i < scribbles.keys.length; i++) {
      var key = scribbles.keys[i];
      var label = new Date(key).toString();
      var item = new goog.ui.MenuItem(label);
      item._key_ = key;
      menu.addChild(item, true);
      scribbleMenuItems.push(item);
    }
  };
  updateMenu();

  var pickle = dom.createDom("pre");
  pickle.style["display"] = "none";
  pickle.style["width"] = "400px";
  pickle.style["height"] = "400px";
  pickle.style["overflow"] = "scroll";
  pickle.style["border"] = "1px solid black";
  dom.appendChild(container, pickle);
  goog.events.listen(scribbles, ble.Scribbles.EventType.UPDATE, updateMenu);
  goog.events.listen(menu, goog.ui.Component.EventType.ACTION, function(e) {

    var target = e.target;
    if(target === stroke) {
      canvas.setMode(0);
    } else if(target === polyline) {
      canvas.setMode(1);
    } else if(target === makeNew) {
      scribbles.makeNew();
      drawCurrent();
    } else if(target === replay) { 
      var replayLength = canvas.painter.data.length * 500;
      canvas.replayAll(replayLength);
    } else if(target === save) {
      scribbles.save(canvas.painter.data);         
    } else if(target == json) {
      dom.removeChildren(pickle);
      var link = dom.createElement("a");
      var data = scribbles.pickle();
      link.href="data:application/json;charset=utf-8," + data;
      dom.appendChild(link, dom.createTextNode("JSON"));
      dom.appendChild(pickle, link);
      dom.appendChild(pickle, dom.createTextNode(scribbles.pickle()));
      pickle.style["display"] = "block";
    } else if(goog.isDef(target._key_)) {
      if(goog.isDef(scribbles.data[target._key_])) {
        scribbles.currentKey = target._key_;
        drawCurrent();
      }
    }
  });


};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
