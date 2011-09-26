goog.provide('ble.use_canvas_3');

goog.require('ble.Scribble');
goog.require('ble.mocap.EventType');
goog.require('ble.mocap.Capture');
goog.require('goog.events');
goog.require('goog.events.EventTarget');
goog.require('goog.ui.Component.EventType');

goog.require('goog.ui.Menu');
goog.require('goog.ui.MenuItem');

goog.require('goog.storage.mechanism.HTML5LocalStorage');

var console = window.console;
var JSON = window.JSON;

/**
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.Scribbles = function() {
  goog.events.EventTarget.call(this);
  this.storage = new goog.storage.mechanism.HTML5LocalStorage();
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
    try {
      var toAdd = ble.mocap.Capture.blessJSONObject(s[i]);
      result.push(toAdd);
    } catch(error) {
      console.log("bless error");
      console.log(error);
    }
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
}

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
  canvas.render(container);

  var scribbles = new ble.Scribbles();

  var drawCurrent = function() {
    if(scribbles.currentKey != null) {
      canvas.scene.complete = scribbles.data[scribbles.currentKey];
      canvas.withContext(canvas.repaintComplete);
    } 
  };

  drawCurrent();

  var menu = new goog.ui.Menu();
  var makeNew = new goog.ui.MenuItem('New');
  var replay = new goog.ui.MenuItem('Replay');
  var save = new goog.ui.MenuItem('Save');
  menu.addChild(save, true);
  menu.addChild(makeNew, true);
  menu.addChild(replay, true);
  menu.addChild(new goog.ui.MenuSeparator(), true);

  menu.render(container);
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
  goog.events.listen(scribbles, ble.Scribbles.EventType.UPDATE, updateMenu);
  goog.events.listen(menu, goog.ui.Component.EventType.ACTION, function(e) {

    var target = e.target;
    if(target === makeNew) {
      scribbles.makeNew();
      drawCurrent();
    } else if(target === replay) { 
      var replayLength = canvas.scene.complete.length * 500;
      canvas.replayAll(replayLength);
    } else if(target === save) {
      scribbles.save(canvas.scene.complete);         
    } else if(goog.isDef(target._key_)) {
      if(goog.isDef(scribbles.data[target._key_])) {
        scribbles.currentKey = target._key_;
        drawCurrent();
      }
    }
  });


};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
