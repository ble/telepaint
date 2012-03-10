
goog.provide('ble.scratch.Sketch');
goog.provide('ble.scratch.StrokeFactory');
goog.require('goog.ui.HsvaPalette');
goog.require('goog.ui.Slider');
goog.require('goog.graphics.CanvasGraphics');
goog.require('goog.graphics.SolidFill');
goog.require('goog.graphics.Stroke');
goog.require('goog.dom');
goog.require('goog.events.Event');
goog.require('goog.style');
/**
 * Factory maintaining width and color/opacity info for creating strokes.
 * @constructor
 * @param {number=} opt_width
 * @param {string=} opt_color
 */
ble.scratch.StrokeFactory = function(opt_width, opt_color) {
  if(!opt_width)
    opt_width = 8;
  if(!opt_color)
    opt_color = "rgba(0, 0, 0, 0.25)";
  this.width = opt_width;
  this.color = opt_color;
  this.listeners = [];
};

ble.scratch.StrokeFactory.prototype.make = function() {
  return new goog.graphics.Stroke(this.width, this.color);
};

ble.scratch.StrokeFactory.prototype.setColor = function(color) {
  this.color = color;
  var L = this.listeners.length;
  for(var i = 0; i < L; i++)
    this.listeners[i].strokeUpdated(this);
}

ble.scratch.StrokeFactory.prototype.setWidth = function(width) {
  this.width = width;
  var L = this.listeners.length;
  for(var i = 0; i < L; i++)
    this.listeners[i].strokeUpdated(this);
}

/**
 * Demonstration of basic sketching using the Closure library.
 * @constructor
 */
  
ble.scratch.Sketch = function(container, preview, picker, width) {
  this.container = container;
  this.preview = preview;
  this.picker = picker;
  this.width = width;
  this.px_width = 480;
  this.px_height = 360;
  this.strokef = new ble.scratch.StrokeFactory();
};

ble.scratch.Sketch.prototype.makeSketchDom = function() {

  if(!this.gfx) {

    var _this = this;
    this.pgfx = new goog.graphics.CanvasGraphics(175, 175);
    this.pgfx.createDom();
    this.pelement = this.pgfx.getElement();

    this.preview.appendChild(this.pelement);
    this.pgfx.enterDocument();
    {
      var p = new goog.graphics.Path();
      p.moveTo(13, 67);
      p.lineTo(162, 110);
      this.prevPath = p;
    }
    {
      var ctx = this.pgfx.getContext();
      ctx.lineCap ="round";
      ctx.lineJoin = "round";
      ctx.save();
    }
    {
      var fillDark = new goog.graphics.SolidFill("#CCC");
      var fillLite = new goog.graphics.SolidFill("#FFF");
      var myStroke = null;
      for(var i = 0; i < 7; i++)
        for(var j = 0; j < 7; j++) {
          var color = ((i + j) % 2 == 1) ? fillLite : fillDark;
          this.pgfx.drawRect(25 * i, 25 * j, 25, 25, myStroke, color);
        }
    }
    this.pdrawn = this.pgfx.drawPath(this.prevPath, this.strokef.make(), null);
    this.strokeUpdated = function(ignored) {
      _this.pdrawn.stroke_ = _this.strokef.make();
      _this.pgfx.redraw();
    }
    this.strokef.listeners.push(this);


    this.gfx = new goog.graphics.CanvasGraphics(this.px_width, this.px_height);
    this.gfx.createDom();
    this.element = this.gfx.getElement();
    this.container.appendChild(this.element);
    this.gfx.enterDocument();





    var thePath = null;
    var isDown = false;
    var lastDrawn = null;
    var downer = function (e) {
      isDown = true;
      thePath = new goog.graphics.Path();
      thePath.moveTo(e.offsetX, e.offsetY);
      var ctx = _this.gfx.getContext();
      ctx.lineCap ="round";
      ctx.lineJoin = "round";
      ctx.save();
      _this.gfx.drawPath(thePath, _this.strokef.make());
    };
    var upper = function (e) {
      isDown = false;
      thePath = null;
    };
    var mover = function(e) {
      if(isDown) {
        thePath.lineTo(e.offsetX, e.offsetY);
        _this.gfx.redraw();
      }
    };
    var enter = function(e) {
      if(isDown) {
        thePath.moveTo(e.offsetX, e.offsetY);
        _this.gfx.redraw();
      }
    };
    goog.events.listen(this.element, goog.events.EventType.MOUSEMOVE, mover);
    goog.events.listen(this.element, goog.events.EventType.MOUSEDOWN, downer);
    goog.events.listen(this.element, goog.events.EventType.MOUSEUP, upper);
    goog.events.listen(this.element, goog.events.EventType.MOUSEOVER, enter);
    //stubbed so I come back to it later.
    goog.events.listen(this.element, goog.events.EventType.TOUCHSTART, function(e) {});


    var pInner = new goog.ui.Component();
    var pColor = new goog.ui.HsvaPalette(null, undefined, undefined, 'goog-hsva-palette-sm');
    pInner.addChild(pColor, true);
    pInner.render();
    this.picker.appendChild(pInner.getElement());
    var onUpdateColor = function(p) {
      var c = goog.color.alpha.hexToRgbaStyle(p.getColorRgbaHex());
      this.strokef.setColor(c);
    }
    goog.events.listen(pColor, goog.ui.Component.EventType.ACTION, goog.partial(onUpdateColor, pColor), false, this);
    pColor.setColorRgbaHex("#ff000088");

    var slider = new goog.ui.Slider();
    slider.setOrientation(goog.ui.Slider.Orientation.VERTICAL);
    slider.createDom();
    var el = slider.getElement();
    el.style.width = '20px';
    el.style.height = '175px';
    slider.setMinimum(1);
    slider.setMaximum(150);
    slider.render();
    goog.dom.appendChild(this.width, slider.getElement());
    var onUpdateWidth = function (s) {
      this.strokef.setWidth(s.getValue());
    }
    goog.events.listen(slider, goog.ui.Component.EventType.CHANGE, goog.partial(onUpdateWidth, slider), false, this);
    

  }

}

ble.scratch.Sketch.prototype.drawThing = function() {
  var stroke = new goog.graphics.Stroke( 5, "#ACF" );
  var fill = new goog.graphics.SolidFill( "#CEF" );
  this.gfx.drawRect(0, 0, this.px_width, this.px_height, stroke, fill);
  this.gfx.drawRect(this.px_width / 4, this.px_height / 4, this.px_width / 2, this.px_height / 2, stroke, fill);
}
