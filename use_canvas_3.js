goog.provide('ble.use_canvas_3');

goog.require('ble.scribble.Canvas');
goog.require('ble.scribble.style.StylePicker');
goog.require('ble.scribble.style.EventType');

goog.require('goog.events');
goog.require('goog.ui.Menu');

ble.use_canvas_3 = function() {

  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var scribble = new ble.scribble.Scribble(640, 480);
  scribble.render(container);

  var scribbles = new ble.Scribbles();
  var createMenu = function() {
    var menu = new goog.ui.Menu();
    var makeNew = new goog.ui.MenuItem('New');
    var replay = new goog.ui.MenuItem('Replay');
    var save = new goog.ui.MenuItem('Save');
    var json = new goog.ui.MenuItem('Display JSON');
    
    menu.addChild(save, true);
    menu.addChild(makeNew, true);
    menu.addChild(replay, true);
    menu.addChild(json, true);
    menu.addChild(new goog.ui.MenuSeparator(), true);
    menu.render(container);
    menu.getElement().style["position"] = "relative";
    menu.getElement().style["display"] = "inline-block";

    if(scribbles.keys.length > 0)
      scribbles.currentKey = scribbles.keys[0];
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

    var drawCurrent = function() {
      if(scribbles.currentKey != null)
        scribble.setPicture(scribbles.data[scribbles.currentKey]);
    };
    goog.events.listen(menu, goog.ui.Component.EventType.ACTION, function(e) {

      var target = e.target;
      if(target === makeNew) {
        scribbles.makeNew();
        drawCurrent();
      } else if(target === replay) { 
        var replayLength = scribble.getPicture().length * 500;
        scribble.canvas.replayAll(replayLength);
      } else if(target === save) {
        scribbles.save(scribble.getPicture());         
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
  createMenu();
};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
