goog.provide('ble.use_canvas_3');

goog.require('ble.Scribble');
goog.require('ble.Scribbles');

goog.require('goog.events');
goog.require('goog.ui.Component.EventType'); 
goog.require('goog.ui.Menu');
goog.require('goog.ui.MenuItem'); 




//var console = window.console;
//var JSON = window.JSON;


ble.use_canvas_3 = function() {
  var pxWidth = 640;
  var pxHeight = 480;

  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var canvas = new ble.Scribble(pxWidth, pxHeight);

  canvas.render(container);
  canvas.getElement().style["border"] = "1px solid black";

  var picker = new ble.scribble.style.StylePicker();
  picker.render(container);

};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
