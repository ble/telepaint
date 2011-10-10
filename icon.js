goog.require('goog.dom.DomHelper');
goog.require('goog.math.Box');
goog.require('goog.events.EventTarget');
goog.require('goog.ui.HsvaPalette');

goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');
goog.require('ble.gfx.StrokeReplay');
goog.require('ble.json.PrettyPrinter');

goog.provide('ble.scribble.icon');

goog.provide('ble.scribble.icon.makeNormalizedStrokeRecorder');
goog.provide('ble.scribble.icon.makeNormalizedPolylineRecorder');
goog.provide('ble.scribble.icon.StylePicker');

ble.scribble.icon.backdropOn = function(ctx, width, height, blockSize) {
  ctx.save();
  var wCount = Math.ceil(width / blockSize);
  var hCount = Math.ceil(height / blockSize);
  var color0 = "#bbb";
  var color1 = "#888";
  for(var i = 0; i < wCount; i++) {
    for(var j = 0; j < hCount; j++) {
      var x = i * blockSize;
      var y = j * blockSize;
      ctx.beginPath();
      ctx.rect(x, y, blockSize, blockSize);
      ctx.fillStyle = ((i + j) % 2 == 0) ? color0 : color1;
      ctx.fill();
    }
  }
  ctx.restore();
};

/**
 * @constructor
 * @extends {goog.ui.Component}
 */
ble.scribble.icon.StylePicker = function() {
  goog.ui.Component.call(this);
};
goog.inherits(ble.scribble.icon.StylePicker, goog.ui.Component);


ble.scribble.icon.StylePicker.prototype.height = 187;

/**
 * Disallow deocartion.
 * @override
 */
ble.scribble.icon.StylePicker.prototype.canDecorate = function(element) {
  return false;
};

ble.scribble.icon.StylePicker.prototype.createDom = function() {
  var domHelper = this.getDomHelper();
  var container = domHelper.createDom('div', {'class': 'ble-scribble-stylepicker'});
  this.setElementInternal(container);

  this.bigIcon = new ble.scratch.Canvas(this.height, this.height);
  this.addChild(this.bigIcon, true);

  var smallSize = Math.floor(this.height / 2);
  var smallIconContainer = new goog.ui.Component();
  var height = this.height;
  smallIconContainer.createDom = function() {
    var domHelper = this.getDomHelper();
    var elt = domHelper.createElement("div");
    elt.style['width'] = height; 
    elt.style['height'] = height; 
    elt.style['display'] = "inline-block";
//    elt.style['margin'] = 0;
//    elt.style['padding'] = 0;
    this.setElementInternal(elt);
  };
  //replace manual styling with css class plz
  this.smallIcons = [];
  for(var i = 0; i < 4; i++) {
    var smallIcon = new ble.scratch.Canvas(smallSize, smallSize);
    smallIconContainer.addChild(smallIcon, true);
    this.smallIcons.push(smallIcon);
  };
  this.smallIconContainer = smallIconContainer;
  this.addChild(smallIconContainer, true);

  this.hsva1 = new goog.ui.HsvaPalette(domHelper, '#000', 1.0, 'goog-hsva-palette-sm');
  this.addChild(this.hsva1, true);

  this.hsva2 = new goog.ui.HsvaPalette(domHelper, '#ffc', 1.0, 'goog-hsva-palette-sm'); 
  this.addChild(this.hsva2, true);
};

ble.scribble.icon.StylePicker.prototype.enterDocument = function() {
  this.bigIcon.withContext(function(ctx) {
    ble.scribble.icon.backdropOn(ctx, 187, 187, 31);
  });

  

  for(var i = 0; i < this.smallIcons.length; i++) {
    var smallIcon = this.smallIcons[i];
    smallIcon.withContext(function(ctx) {
      ble.scribble.icon.backdropOn(ctx, 93, 93, 16);
    });
  }
};
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
 * @param{number} scale
 * @param{Array.<number>} coordinates
 * @return{Array.<number>}
 */
ble.scribble.icon.scaleUp = function(scale, coordinates) {
  var result = coordinates.slice();
  for(var i = 0; i < result.length; i++) {
    result[i] = Math.round(result[i] * scale);
  }
  return result;
};

/**
 * @param{HTMLElement} container
 * @param{number} size
 * @param{ble.mocap.Mocap} mocap
 * @param{function(ble.mocap.Capture, ble.gfx.path.Painter): ble.gfx.DrawPart} converter 
 * @param{ble.gfx.path.Painter} style
 * @return{ble.scratch.Canvas}
 */ 
ble.scribble.icon.makeRecorder = function(container, size, mocap, converter, style) {
  var domHelper = new goog.dom.DomHelper();
  var canvas = new ble.scratch.Canvas(size, size);
  canvas.render(container);
  var jsonContainer = domHelper.createDom("pre", null);
  jsonContainer.style['width'] = "300px";
  jsonContainer.style['overflow-x'] = "scroll";
  domHelper.appendChild(container, jsonContainer);

  var pxBox = new goog.math.Box(0, size, size, 0);
  var vBox = new goog.math.Box(0, 1, 1, 0);

  canvas.getElement().style['border'] = "1px solid red";
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

  var prettyPrinter = new ble.json.PrettyPrinter();

  //Wire motion capture events to drawing on the subcanvas... 
  goog.events.listen(
      mocap,
      ble.mocap.EventType.ALL,
      function(event) {
        var toPaint = converter(event.capture, style);      
        var end = (event.type == ble.mocap.EventType.END);
        canvas.withContext(function(context) {
          context.clearRect(0, 0, 1, 1);
        });
        subcanvas.withContext(function(context) {
          toPaint.drawCompleteTo(context);
        });
        if(end) {
          jsonContainer.innerHTML = prettyPrinter.serialize(toPaint);
        }
      });
  return canvas;
};

/**
 * @param{HTMLElement} container
 * @param{number} size
 * @return{ble.scratch.Canvas}
 */
ble.scribble.icon.strokeRecorder = function(container, size) {
  var mocap = new ble.mocap.Stroke();
  var converter = ble.gfx.StrokeReplay.fromMocap;
  var style = new ble.gfx.path.PainterPixel(1.5, "#000");
  return ble.scribble.icon.makeRecorder(container, size, mocap, converter, style);
};

/**
 * @param{HTMLElement} container
 * @param{number} size
 * @return{ble.scratch.Canvas}
 */ 
ble.scribble.icon.polylineRecorder = function(container, size) {
  var mocap = new ble.mocap.Polyline(true);
  var converter = ble.gfx.PolylineReplay.fromMocap;
  var style = new ble.gfx.path.PainterPixel(1.5, "#000");
  return ble.scribble.icon.makeRecorder(container, size, mocap, converter, style); 
};

goog.exportSymbol('ble.scribble.icon.strokeRecorder', ble.scribble.icon.strokeRecorder); 
goog.exportSymbol('ble.scribble.icon.polylineRecorder', ble.scribble.icon.polylineRecorder); 
