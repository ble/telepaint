goog.provide('ble.scribble.Canvas');
goog.provide('ble.scribble.Painter');
goog.provide('ble.scribble.UI');
goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.Stroke');
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
 * @param {Array.<ble._2d.DrawPart>} items
 * @implements {ble._2d.DrawPart}
 */
ble.scribble.Drawing = function(startTime, items) {
  this.fetcher = new ble.interval.Fetcher(items);
  this.byStart = items.slice();
  this.byStart.sort(this.compareStart_);
  this.startTime = startTime;
};

ble.scribble.Drawing.prototype.compareStart_ =
  ble.util.comparatorFromRank(ble.interval.startRank);

/**
 * @param {ble._2d.DrawPart} item
 */
ble.scribble.Drawing.prototype.add = function(item) {
  this.fetcher.add(item);
  ble.util.rankBinaryInsert(ble.interval.startRank, this.byStart, item);
};

ble.scribble.Drawing.prototype.start = function() {
  return this.startTime;
};

ble.scribble.Drawing.prototype.end = function() {
  return this.start() + this.length();
};

ble.scribble.Drawing.prototype.length = function() {
  return this.fetcher.byEnd[this.fetcher.byEnd.length - 1].end();
};

ble.scribble.Drawing.prototype.withStartAt = function(newTime) {
  return new ble.scribble.Drawing(newTime, this.byStart);
};

ble.scribble.Drawing.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var scale = function(i0) {
    var i1 = i0.withStartAt(i0.start() * scaleFactor);
    var i2 = i1.withLength(i1.length() * scaleFactor);
    return i2;
  };
  var byStart = this.byStart.map(scale);
  return new ble.scribble.Drawing(this.startTime, byStart);
};

ble.scribble.Drawing.prototype.draw = function(ctx) {
  for(var i = 0; i < this.byStart.length; i++) {
    this.byStart[i].draw(ctx);
  }
};

ble.scribble.Drawing.prototype.at = function(time) {
  time -= this.start();
  if(time < 0)
    return ble._2d.nothing;
  if(time >= this.length())
    return this;

  //Assembling all of the right Drawables in the right order is a little 
  //tricky; they must all be sorted according to the start of their interval,
  //but the DrawParts that are only partially complete must be converted into
  //Drawables which do not have any interval information.

  //The approach used is to add a (hopefully/almost certainly unique) field to
  //the DrawParts which must be converted, put all DrawParts into one array
  //and sort by start, then replace each flagged item in the array with the
  //partially complete item and remove the flag from the Drawable.

  //There are more elegant ways of doing this, primarily by having the partial
  //Drawables carry the interval information.  The elegant way of doing that
  //is to set the prototype of a newly-created partial Drawable to be the
  //instance from which it was created.  Unfortunately, neither __proto__ nor
  //Object.create() are guaranteed to work on all browsers of interest, and I
  //find the
  /*
     function inherit(proto) {
      function F() {}
      F.prototype = proto
      return new F();
    };
  */
  //approach intriguing but uncertain.  It'll be nice when one can assume
  //that Object.create() is present on any browser; prototypal OO is much less
  //compelling when you can't make an object's prototype an instance rather
  //than a constructor.

  var paired = this.fetcher.beforeAndDuring(time);
  var during = paired[1];

  //Flag all of the items which will be partially drawn
  during.map(function(item) { item.__duringFlag__ = true; });

  //Concatenate and sort all the items
  var toDraw = paired[0].concat(during);
  toDraw.sort(this.compareStart_);

  //Replace all the items in the array which should be partially drawn with
  //their partial forms and unflag the original items.
  goog.array.forEach(
    toDraw,
    function(value, index, array) {
      if(value.__duringFlag__) {
        array[index] = value.at(time);
        delete(value.__duringFlag__);
      }
    });

  var draw = function(items, N, ctx) {
    for(var i = 0; i < N; i++) {
      items[i].draw(ctx);
    }
  };
  var drawable = new ble._2d.Nothing();
  drawable.draw = goog.partial(draw, toDraw, toDraw.length); 
  return drawable;
};
/**
 * @constructor
 * @param {Array.<ble._2d.DrawPart>} items
 * @implements {ble._2d.DrawPart}
 */
ble.scribble.Painter = function(items) {
  this.data = items.slice();
  this.paintReady = this.zeroReference(this.data);
  this.beingDrawn = null;
};

ble.scribble.Painter.prototype.startTime = function() {
  return this.paintReady.length > 0 ? this.paintReady[0].startTime()
                                    : 0;
};

ble.scribble.Painter.prototype.endTime = function() {
  return this.paintReady.length > 0 ? this.paintReady[this.paintReady.length - 1].endTime()
                                    : 0;
};

ble.scribble.Painter.prototype.withStartTime = function(newStart) {
  var result = new ble.scribble.Painter(this.data);
  var toAlter = result.paintReady;
  var lastEnd = newStart;
  for(var i = 0; i < toAlter.length; i++) {
    toAlter[i] = toAlter[i].withStartTime(lastEnd);
    lastEnd = toAlter[i].endTime();
  }
  return result;
};

ble.scribble.Painter.prototype.toJSON = function() {
  return this.data;
};

/**
 * @param {Array.<ble._2d.DrawPart>} parts
 */
ble.scribble.Painter.prototype.zeroReference = function(parts) {
  var result = [];
  var lastEnd = 0;
  for(var i = 0; i < parts.length; i++) {
    result[i] = parts[i].withStartTime(lastEnd);
    lastEnd = result[i].endTime();
  };
  return result;
};

/**
 * @param {ble._2d.DrawPart} current
 */
ble.scribble.Painter.prototype.setCurrent = function(current) {
  this.beingDrawn = current;
};

ble.scribble.Painter.prototype.recordCurrent = function() {
  var current = this.beingDrawn;
  if(!goog.isNull(current)) {
    this.data.push(current);
    var adjusted = current.withStartTime(this.endTime());
    this.paintReady.push(adjusted);
    this.beingDrawn = null; 
  }
};

ble.scribble.Painter.prototype.drawCompleteTo = function(ctx) {
  for(var i = 0; i < this.paintReady.length; i++)
    this.paintReady[i].drawCompleteTo(ctx); 
  if(!goog.isNull(this.beingDrawn)) {
    this.beingDrawn.drawCompleteTo(ctx);
  }
};

ble.scribble.Painter.prototype.drawPartialTo = function(time, ctx) {
  if(!goog.isNull(this.beingDrawn)) {
    this.beingDrawn.drawCompleteTo(ctx);
  }
  for(var i = 0; i < this.paintReady.length; i++) {
    var item = this.paintReady[i];
    if(item.startTime() > time)
      break;
    item.drawPartialTo(time, ctx);
  } 
};

/**
 * @constructor
 * @param {number} width
 * @param {number} height
 * @param {ble.scribble.Painter=} opt_painter
 * @extends {ble.scratch.Canvas}
 */
ble.scribble.Canvas = function(width, height, opt_painter) {
  ble.scratch.Canvas.call(this, width, height);
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
  else
    this.painter = new ble.scribble.Painter([]);
  this.mocap_ = null;
  this.modes = this.makeModes();
  this.style = ble._2d.path.painterDefault;
};
goog.inherits(ble.scribble.Canvas, ble.scratch.Canvas);

ble.scribble.Canvas.prototype.repaintComplete = function(ctx) {
  ctx.clearRect(0, 0, this.width_px, this.height_px);
  this.painter.drawCompleteTo(ctx);
};

ble.scribble.Canvas.prototype.repaintAt = function(time) {
  return function(ctx) { 
    ctx.clearRect(0, 0, this.width_px, this.height_px);
    this.painter.drawPartialTo(time, ctx);
  };
};

ble.scribble.Canvas.prototype.finishAnimation = function() {
  this.animating = false;
};

ble.scribble.Canvas.prototype.replayAll = function(duration_millis) {
  if(this.animating)
    return;
  this.animating = true;
  var real_duration = this.painter.endTime() - this.painter.startTime();
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
      var effective_time = capture_dur * (delta / replay_dur);
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
      var effective_time = capture_dur * (delta / replay_dur);
      this.withContext(this.repaintAt(effective_time));
    }
  }, this);
  handle = window.setInterval(redraw, interval); 
};

ble.scribble.Canvas.prototype.handleEvent = function(event) {
  goog.base(this, "handleEvent", event);
  if(event.propagationStopped_)
    return;
  var painter = this.painter;
  if(event.type == ble.mocap.EventType.BEGIN) {
    painter.setCurrent(this.converter(event.capture, this.style));
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
    painter.setCurrent(this.converter(event.capture, this.style));
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.END) {
    painter.setCurrent(this.converter(event.capture, this.style));
    painter.recordCurrent();
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

ble.scribble.UI.prototype.setPicture = function(data) {
  this.canvas.painter = new ble.scribble.Painter(data);
  this.canvas.withContext(goog.bind(this.canvas.repaintComplete, this.canvas));
};

ble.scribble.UI.prototype.getPicture = function() {
  return this.canvas.painter.data;
};
