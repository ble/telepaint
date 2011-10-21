goog.provide('ble.use_canvas_3');

goog.require('ble.scribble.Canvas');
goog.require('ble.scribble.style.StylePicker');
goog.require('ble.scribble.style.EventType');

goog.require('goog.events');

ble.use_canvas_3 = function() {
  var pxWidth = 640;
  var pxHeight = 480;

  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var canvas = new ble.scribble.Canvas(pxWidth, pxHeight);

  canvas.render(container);
  canvas.getElement().style["border"] = "1px solid black";

  var picker = new ble.scribble.style.StylePicker();
  picker.render(container);

  goog.events.listen(
      picker,
      ble.scribble.style.EventType.STYLECHANGED,
      function(e) {
        this.setStyle(e.style);
      },
      false,
      canvas);
  goog.events.listen(
      picker,
      ble.scribble.style.EventType.METHODCHANGED,
      function(e) {
        this.setMode(e.method);
        this.setStyle(e.style);
      },
      false,
      canvas);
};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
