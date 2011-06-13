

goog.provide('ble.mocap.Mocap');
goog.provide('ble.mocap.Replayer');
goog.provide('ble.mocap.EventType');

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
 */

ble.mocap.Mocap = function() {
  this.startTime = null;
  this.capture = null;
  /**
   * Targets which will receive mocap events.
   * @type {Object.<string,Array.<goog.events.EventTarget>}
   */
  this.targets = {};
}

ble.mocap.Mocap.prototype.dispatchMocap = function(type) {
  var targets = this.targets[type];
  if(goog.isNull(targets))
    return;
  var dispatchMocap = new goog.events.Event(type);
  dispatchMocap.capture = this.capture;
  for(var i = 0; i < targets.length; i++) {
    var result = targets[i].dispatchEvent(dispatchMocap);
    if(result === false)
      return;
  }
}

ble.mocap.Mocap.prototype.forwardingListener = goog.abstractMethod;

/**
 * @param {goog.events.EventTarget} target
 * @param {string} type
 */
ble.mocap.Mocap.prototype.addTarget = function(target, type) {
  if(goog.isArray(type)) {
    for(var i = 0; i < type.length; i++)
      this.addTarget(target, type[i]);
  } else {
    if(!(type in this.targets))
      this.targets[type] = [];
    var targets = this.targets[type];
    for(var i = 0; i < targets.length; i++) {
      if(targets[i] === target)
        return;
    }
    targets.unshift(target);
  }
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
