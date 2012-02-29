goog.provide('ble.use_canvas_3');

goog.require('ble.scribble.Simultaneous');
goog.require('ble.scribble.Canvas');
goog.require('ble.scribble.style.StylePicker');
goog.require('ble.scribble.style.EventType');

goog.require('goog.events');
goog.require('goog.events.EventHandler');
goog.require('goog.ui.Menu');

goog.require('goog.math.Vec2');
goog.require('ble.traj');

ble.use_canvas_3 = function() {
  var drawingSize = new goog.math.Size(640, 480);
  var smallDrawingSize = new goog.math.Size(213, 160);
  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var scribble = new ble.scribble.UI(drawingSize.width, drawingSize.height);
  scribble.render(container);
  var handlers = {foo: 2};
  handlers.end = function(event) {
    console.log(event.capture);
    console.log(this.foo);
    var target = event.target;
    var drawing = target.drawing;
    var toAdd = drawing.getCurrent();
    drawing.setCurrent(null);
    target.withContext(target.repaintComplete);
    window.setTimeout(function() {
      var tmp = drawing.getCurrent();
      drawing.setCurrent(toAdd);
      drawing.recordCurrent();
      drawing.setCurrent(tmp);
      target.withContext(target.repaintComplete);
    }, 1000);
    return false;
  };
  var handler = new goog.events.EventHandler(handlers);
  handler.listen(scribble, ble.scribble.Canvas.EventType.END, handlers.end);
};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
