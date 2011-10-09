goog.provide('ble.use_canvas_3');

goog.require('ble.Scribble');
goog.require('ble.Scribbles');

goog.require('goog.events');
goog.require('goog.ui.Component.EventType'); 
goog.require('goog.ui.Menu');
goog.require('goog.ui.MenuItem'); 




var console = window.console;
var JSON = window.JSON;


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
