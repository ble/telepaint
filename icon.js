goog.require('goog.math.Box');
goog.require('goog.events.EventTarget');

goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');
goog.require('ble.gfx.StrokeReplay');

goog.provide('ble.scribble.icon');

goog.provide('ble.scribble.icon.makeNormalizedStrokeRecorder');
/**
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.scribble.icon.Style = function(color1, color2, strokeWidth) {
  goog.events.EventTarget.call(this);
  this.color1 = color1;
  this.color2 = color2;
  this.strokeWidth = strokeWidth;
};
goog.inherits(ble.scribble.icon.Style, goog.events.EventTarget);

ble.scribble.icon.fixer = function(coordinates) {
  var result = coordinates.slice();
  for(var i = 0; i < result.length; i++) 
    result[i] = Math.round(result[i]);
  return result; 
};

/**
 * @param{HTMLElement} container
 * @param{number} size
 */
ble.scribble.icon.makeNormalizedStrokeRecorder = function(container, size) {
  var canvas = new ble.scratch.Canvas(size, size);
  canvas.render(container);
  var pxBox = new goog.math.Box(0, size, size, 0);
  var vBox = new goog.math.Box(0, 1, 1, 0);
  var mocap = new ble.mocap.Stroke();
  canvas.getElement().style['border'] = "2px solid red";
  canvas.getElement().style['display'] = "inline-block";
  //Wire mouse events on canvas element to canvas controller...
  goog.events.listen(
      canvas.getElement(),
      mocap.eventTypesOfInterest,
      canvas);
  //Wire canvas events to subcanvas...
  var subcanvas = new ble.scratch.Subcanvas(canvas, pxBox, vBox, true);
  canvas.forwardEvents(subcanvas, mocap.eventTypesOfInterest);
  //Wire subcanvas events to motion capture...
  goog.events.listen(
      subcanvas,
      mocap.eventTypesOfInterest,
      mocap);
  //Wire motion capture events to drawing on the subcanvas... 
  goog.events.listen(
      mocap,
      ble.mocap.EventType.ALL,
      function(event) {
        
        var stroke = ble.gfx.StrokeReplay.fromMocap(event.capture);
        canvas.withContext(function(context) {
          context.clearRect(0, 0, size, size);
        });
        subcanvas.withContext(function(context) {
          stroke.drawCompleteTo(context);
        });
      });
}

goog.exportSymbol('ble.scribble.icon.makeNormalizedStrokeRecorder', ble.scribble.icon.makeNormalizedStrokeRecorder); 
