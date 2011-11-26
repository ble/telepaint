goog.provide('ble.scribble.Canvas');
goog.provide('ble.scribble.UI');

goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.Stroke');
goog.require('ble.scribble.MutableDrawing');

goog.require('ble._2d');
goog.require('ble._2d.path.painterDefault');
goog.require('ble._2d.PolylineReplay');
goog.require('ble._2d.DrawPart');
goog.require('ble.interval.Fetcher');
goog.require('ble.interval.startRank');
goog.require('goog.events');
goog.require('goog.events.EventType');

/**
 * @constructor
 * @param {number} width
 * @param {number} height
 * @param {ble.scribble.MutableDrawing=} opt_drawing
 * @extends {ble.scratch.Canvas}
 */
ble.scribble.Canvas = function(width, height, opt_drawing) {
  ble.scratch.Canvas.call(this, width, height);
  if(goog.isDef(opt_drawing))
    this.drawing = opt_drawing;
  else
    this.drawing = new ble.scribble.MutableDrawing(Date.now(), []);
  this.mocap_ = null;
  this.modes = this.makeModes();
  this.style = ble._2d.path.painterDefault;
};
goog.inherits(ble.scribble.Canvas, ble.scratch.Canvas);

ble.scribble.Canvas.prototype.repaintComplete = function(ctx) {
  ctx.clearRect(0, 0, this.width_px, this.height_px);
  this.drawing.draw(ctx);
};

ble.scribble.Canvas.prototype.repaintAt = function(time) {
  return function(ctx) { 
    ctx.clearRect(0, 0, this.width_px, this.height_px);
    this.drawing.at(time).draw(ctx);
  };
};

ble.scribble.Canvas.prototype.finishAnimation = function() {
  this.animating = false;
};

ble.scribble.Canvas.prototype.replayAll = function(duration_millis) {
  if(this.animating)
    return;
  this.animating = true;
  var real_duration = this.drawing.length();
  if(window.webkitRequestAnimationFrame) {
    this.animateRAF(duration_millis, real_duration);
  } else {
    this.animateInterval(duration_millis, real_duration, 32); 
  }
};

ble.scribble.Canvas.prototype.animateRAF = function(replay_dur, capture_dur) {
  var start = Date.now();
  var redraw = goog.bind(function(now) {
    var delta = now - start;
    if(delta > replay_dur) {
      this.withContext(this.repaintComplete);
      this.finishAnimation();
    } else {
      var effective_time = capture_dur * (delta / replay_dur) + this.drawing.start();
      this.withContext(this.repaintAt(effective_time));
      window.webkitRequestAnimationFrame(redraw);
    }
  }, this);
  redraw(Date.now());
};

ble.scribble.Canvas.prototype.animateInterval = function(replay_dur, capture_dur, interval) {
  var start = Date.now();
  var handle;
  var redraw = goog.bind(function() {
    var now = Date.now();
    var delta = now - start;
    if(delta > replay_dur) {
      this.withContext(this.repaintComplete);
      this.finishAnimation();
      window.clearInterval(handle);
    } else {
      var effective_time = capture_dur * (delta / replay_dur) + this.drawing.start();
      this.withContext(this.repaintAt(effective_time));
    }
  }, this);
  handle = window.setInterval(redraw, interval); 
};

ble.scribble.Canvas.prototype.handleEvent = function(event) {
  goog.base(this, "handleEvent", event);
  if(event.propagationStopped_)
    return;
  var drawing = this.drawing;
  if(event.type == ble.mocap.EventType.BEGIN) {
    drawing.setCurrent(this.converter(event.capture, this.style));
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
    drawing.setCurrent(this.converter(event.capture, this.style));
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.END) {
    drawing.setCurrent(this.converter(event.capture, this.style));
    drawing.recordCurrent();
    this.dispatchEvent(event);
  }

};


ble.scribble.Canvas.prototype.makeModes = function() {
  var stroke = new ble.mocap.Stroke();
  var polyline = new ble.mocap.Polyline(true);
  return [
    [stroke, ble._2d.StrokeReplay.fromMocap],
    [polyline, ble._2d.PolylineReplay.fromMocap],
    [stroke, ble._2d.EraseReplay.fromMocap],
    [polyline, ble._2d.PolylineReplay.fromMocap]
  ]; 
};

ble.scribble.Canvas.prototype.enterDocument = function() {
  this.setMode(0);
};

ble.scribble.Canvas.prototype.setStyle = function(style) {
  this.style = style;
};

ble.scribble.Canvas.prototype.setMode = function(modeNum) {
  if(!goog.isNull(this.mocap_)) {
    goog.events.unlisten(
        this.getElement(),
        this.mocap_.eventTypesOfInterest,
        this.mocap_);
    goog.events.unlisten(
        this.mocap_,
        ble.mocap.EventType.ALL,
        this);
    this.mocap_ = null;
  }
  var mode = this.modes[modeNum];
  this.mocap_ = mode[0];
  this.converter = mode[1];
  goog.events.listen(
      this.getElement(),
      this.mocap_.eventTypesOfInterest,
      this.mocap_);
  goog.events.listen(
      this.mocap_,
      ble.mocap.EventType.ALL,
      this);

};

ble.scribble.Canvas.prototype.exitDocument = function() {
  if(goog.isDef(this.mocap_)) {
    var motionCapture = this.mocap_;
    goog.events.unlisten(
      this.getElement(),
      motionCapture.eventTypesOfInterest,
      motionCapture);

    goog.events.unlisten(
      motionCapture,
      ble.mocap.EventType.ALL,
      this);
    this.mocap_ = undefined;
  }
};

/**
 * @constructor
 * @param{number} width
 * @param{number} height
 * @extends{goog.ui.Component}
 */
ble.scribble.UI = function(width, height) {
  this.width = width;
  this.height = height;
  this.listenerKeys = [];
  goog.ui.Component.call(this);
};
goog.inherits(ble.scribble.UI, goog.ui.Component);

ble.scribble.UI.prototype.createDom = function() {
  var domHelper = this.getDomHelper();
  var container = domHelper.createDom('div', {'class': 'ble-scribble-stylepicker'});
  this.setElementInternal(container);

  this.canvas = new ble.scribble.Canvas(this.width, this.height);
  this.picker = new ble.scribble.style.StylePicker();
  this.addChild(this.canvas, true);
  this.addChild(this.picker, true); 
};

ble.scribble.UI.prototype.enterDocument = function() {
  goog.base(this, 'enterDocument');
  this.canvas.getElement().style['border'] = '1px solid black';
  var ctx = this.canvas.getRawContext();
  ctx.lineJoin = "round";
  ctx.lineCap = "round";
  this.listenerKeys.push(goog.events.listen(
      this.picker,
      ble.scribble.style.EventType.STYLECHANGED,
      function(e) {
        this.setStyle(e.style);
      },
      false,
      this.canvas));
  this.listenerKeys.push(goog.events.listen(
      this.picker,
      ble.scribble.style.EventType.METHODCHANGED,
      function(e) {
        this.setMode(e.method);
        this.setStyle(e.style);
      },
      false,
      this.canvas));
};

ble.scribble.UI.prototype.exitDocument = function() {
  while(this.listenerKeys.length > 0) {
    var key = this.listenerKeys.pop();
    goog.events.unlistenByKey(key);
  }
  goog.base(this, 'exitDocument');
};

ble.scribble.UI.prototype.setPicture = function(startTime, data) {
  this.canvas.drawing = new ble.scribble.MutableDrawing(startTime, data);
  this.canvas.withContext(goog.bind(this.canvas.repaintComplete, this.canvas));
};

ble.scribble.UI.prototype.getPicture = function() {
  return this.canvas.drawing.byStart;
};
