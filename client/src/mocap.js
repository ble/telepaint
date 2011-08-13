

goog.provide('ble.mocap.Mocap');
goog.provide('ble.mocap.Replayer');
goog.provide('ble.mocap.EventType');
goog.provide('ble.mocap.Capture');
goog.provide('ble.mocap.Stroke');
goog.provide('ble.mocap.Polyline');

goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.events.EventTarget');
goog.require('goog.events.Event');

/**
 * Constants for event names
 * @enum {string}
 */
ble.mocap.EventType = {
  BEGIN: 'begin',
  PROGRESS: 'progress',
  CONTROLPOINT: 'controlpoint',
  END: 'end'
};

ble.mocap.EventType.ALL =
  [ble.mocap.EventType.BEGIN,
  ble.mocap.EventType.PROGRESS,
  ble.mocap.EventType.CONTROLPOINT,
  ble.mocap.EventType.END];
  

/**
 * A motion-capture object.
 * @param {number} startTime
 * @constructor
 */
ble.mocap.Capture = function(startTime) {
  this.startTime = startTime;
  this.coordinates = [];
  this.times = [];
  this.controlTimeIndices = [];
  this.controlPoints = [];
}

ble.mocap.Capture.prototype.getControlCoordinates = function() {
  var result = [];
  for(var i = 0; i < this.controlTimeIndices.length; i++) {
    var ix = this.controlTimeIndices[i];
    result.push(this.coordinates[2*ix], this.coordinates[2*ix+1]);
  }
  return result;
};

ble.mocap.Capture.prototype.getControlCoordinatesAndHead = function() {
  var result = this.getControlCoordinates();
  var L = this.coordinates.length;
  var C = this.coordinates;
  result.push(C[L-2], C[L-1]);
  return result;
};

ble.mocap.Capture.blessJSONObject = function(obj) {
  var result = new ble.mocap.Capture(obj.startTime);
  result.coordinates = obj.coordinates;
  result.times = obj.times;
  result.controlTimeIndices = obj.controlTimeIndices;
  result.controlPoints = obj.controlPoints;
  delete obj.coordinates;
  delete obj.times;
  delete obj.controlTimeIndices;
  delete obj.controlPoints;
  return result;
}

/**
 * Concatenate a regular point to the capture.
 * @param {number} time
 * @param {number} x
 * @param {number} y
 */
ble.mocap.Capture.prototype.addCoordinates = function(time, x, y) {
  this.times.push(time - this.startTime);
  this.coordinates.push(x, y);
}

/**
 * Concatenate a control point to the capture.
 * @param {number} time
 * @param {number} x
 * @param {number} y
 * @param {*} controlValue
 */
ble.mocap.Capture.prototype.addControl = function(time, x, y, controlValue) {
  this.times.push(time - this.startTime);
  this.coordinates.push(x, y);
  this.controlTimeIndices.push(this.times.length - 1);
  this.controlPoints.push(controlValue);
}

/**
 * Time-slice the capture.
 * @param {number} time0
 * @param {number} time1
 * @return {Array.<number>}
 */
ble.mocap.Capture.prototype.betweenTimes = function(time0, time1) {
  var ix0 = Math.ceil(ble.mocap.binarySearch(this.times, time0));
  var ix1 = Math.floor(ble.mocap.binarySearch(this.times, time1));
  var cIx0 = Math.ceil(ble.mocap.binarySearch(this.controlTimeIndices, ix0));
  var cIx1 = Math.floor(ble.mocap.binarySearch(this.controlTimeIndices, ix1));
  return [ix0, ix1, cIx0, cIx1];
}

ble.mocap.binarySearch = function(array, value) {
  if(array.length == 0)
    return -1;
  return ble.mocap.binarySearch0(array, 0, array.length-1, Math.floor(array.length / 2), value);
};

ble.mocap.binarySearch0 = function(array, ix0, ixF, ix, value) {
  var ixVal = array[ix];
  if(ixVal == value) {
    return ix;
  }
  
  else if(ix0 == ix || ixF == ix) {
    if(ixVal > value)
      return ix - 0.5;
    if(ixVal < value)
      return ix + 0.5;
  }

  else if(ixVal > value) {
    return ble.mocap.binarySearch0(array, ix0, ix, Math.floor((ix0 + ix) / 2), value);
  }  else { 
    return ble.mocap.binarySearch0(array, ix, ixF, Math.ceil((ix + ixF) / 2), value);
  }
}

/**
 * Abstract class providing motion-capture functionality for mouse events.
 * @constructor
 * @extends {goog.events.EventTarget}
 */

ble.mocap.Mocap = function() {
  this.startTime = null;
  this.capture = null;
  goog.events.EventTarget.call(this);
}
goog.inherits(ble.mocap.Mocap, goog.events.EventTarget);

ble.mocap.Mocap.prototype.dispatchMocap = function(type) {
  var event = new goog.events.Event(type);
  event.capture = this.capture;
  this.dispatchEvent(event);
}

ble.mocap.Mocap.prototype.beginCapture = function(event) {
  var time = event.getBrowserEvent().timeStamp;
  this.capture = new ble.mocap.Capture(time);
  this.capture.addCoordinates(time, event.offsetX, event.offsetY);
  this.dispatchMocap(ble.mocap.EventType.BEGIN);
}

ble.mocap.Mocap.prototype.progressCapture = function(event) {
  var time = event.getBrowserEvent().timeStamp;
  this.capture.addCoordinates(time, event.offsetX, event.offsetY);
  this.dispatchMocap(ble.mocap.EventType.PROGRESS);
}

ble.mocap.Mocap.prototype.controlCapture = function(event, controlValue) {
  var time = event.getBrowserEvent().timeStamp;
  this.capture.addControl(time, event.offsetX, event.offsetY, controlValue);
  this.dispatchMocap(ble.mocap.EventType.CONTROLPOINT);
}

ble.mocap.Mocap.prototype.endCapture = function(event) {
  var time = event.getBrowserEvent().timeStamp;
  this.capture.addCoordinates(time, event.offsetX, event.offsetY);
  this.dispatchMocap(ble.mocap.EventType.END);
  this.capture = null;
}

/**
 * Stroke-based motion capture: capture begins on mousedown, progresses with
 * each mousemove until a mouseup event.
 * @constructor
 * @extends {ble.mocap.Mocap}
 */
ble.mocap.Stroke = function() {
  ble.mocap.Mocap.call(this);
  this.midStroke = false;
};
goog.inherits(ble.mocap.Stroke, ble.mocap.Mocap);

ble.mocap.Stroke.prototype.forwardingListener = function(event) {
  if(this.midStroke) {
    if(event.type === goog.events.EventType.MOUSEMOVE) {
      this.progressCapture(event);
    } else if(event.type === goog.events.EventType.MOUSEUP ||
              event.type === goog.events.EventType.MOUSEOUT) {
      this.endCapture(event);
      this.midStroke = false;
    }
  } else if(event.type === goog.events.EventType.MOUSEDOWN) {
    this.midStroke = true;
    this.beginCapture(event);
  } 
}

ble.mocap.Stroke.prototype.eventTypesOfInterest =
  [goog.events.EventType.MOUSEDOWN,
   goog.events.EventType.MOUSEMOVE,
   goog.events.EventType.MOUSEUP,
   goog.events.EventType.MOUSEOUT];

/**
 * Polyline motion capture: capture begins on click, optionally progresses
 * with mousemove, control points on subsequent clicks, ends on dblclick.
 * @param {boolean?} captureMove
 * @constructor
 * @extends {ble.mocap.Mocap}
 */
ble.mocap.Polyline = function(captureMove) {
  ble.mocap.Mocap.call(this);
  this.drawing = false;
  this.captureMove = captureMove;
  this.eventTypesOfInterest = this.eventTypesOfInterest0();
};
goog.inherits(ble.mocap.Polyline, ble.mocap.Mocap);

ble.mocap.Polyline.prototype.eventTypesOfInterest0 = function() {
  var types = [goog.events.EventType.CLICK, goog.events.EventType.DBLCLICK];
  if(this.captureMove)
    types.push(goog.events.EventType.MOUSEMOVE);
  return types;
};

ble.mocap.Polyline.prototype.forwardingListener = function(event) {
  if(this.drawing) {
    if(this.captureMove && event.type == goog.events.EventType.MOUSEMOVE) {
      this.progressCapture(event);
    } else if(event.type == goog.events.EventType.CLICK) {
      this.controlCapture(event, null);
    } else if(event.type == goog.events.EventType.DBLCLICK) {
      this.controlCapture(event, null);
      this.endCapture(event);
      this.drawing = false;
    }
  } else if(event.type == goog.events.EventType.CLICK) {
    this.drawing = true;
    this.beginCapture(event);
    this.controlCapture(event, null);
  }
}
