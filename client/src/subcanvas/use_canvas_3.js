goog.provide('ble.use_canvas_3');

goog.require('goog.math.Size');
goog.require('goog.dom.DomHelper');

goog.require('ble.game.GroupDraw');

ble.use_canvas_3 = function() {
  var drawingSize = new goog.math.Size(640, 480);
  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");

  var game = new ble.game.GroupDraw(drawingSize.width, drawingSize.height, null);
  game.render(container);
};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
