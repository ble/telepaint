goog.provide('ble.use_canvas_3');

goog.require('ble.scribble.Canvas');
goog.require('ble.scribble.style.StylePicker');
goog.require('ble.scribble.style.EventType');

goog.require('goog.events');

ble.use_canvas_3 = function() {

  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var scribble = new ble.scribble.Scribble(640, 480);
  scribble.render(container);
};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
