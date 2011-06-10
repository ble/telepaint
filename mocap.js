/**
 * @fileoverview Mouse event listening which records time information with
 * coordinates, allowing for replay or analysis of user gestures.
 * @author benjaminster@gmail.com (Ben Ellis)
 */

goog.provide('ble.mocap.PixelMocap');
goog.provide('ble.mocap.Replayer');

goog.require('goog.events');
goog.require('goog.events.EventType');

/**
 * A motion-capture listener which records the location and time-after-
 * mousedown of every mousemove event after mousedown and before mouseup or
 * mouseout.
 * @param {function(Array.<number>)} callbackDone executed on mouseup / mouseout.
 * @constructor
 */
ble.mocap.PixelMocap = function(callbackMoved, callbackDone) {
  this.callbackMoved = callbackMoved;
  this.callbackDone = callbackDone;
  this.mouseState = this.UP;
  this.startTime = null;
  this.coordinates = null;
};

ble.mocap.PixelMocap.prototype.DOWN = "down";
ble.mocap.PixelMocap.prototype.UP = "UP";

ble.mocap.PixelMocap.prototype.handler = function() {
  return goog.bind(this.handler0, this);
};

ble.mocap.PixelMocap.prototype.handler0 = function(event) {
  if(event.type == goog.events.EventType.MOUSEDOWN) {
    this.initMocap(event);
    this.mocap(event);
    this.callbackMoved(event);
  } else if(this.mouseState == this.DOWN &&
            (event.type == goog.events.EventType.MOUSEUP ||
             event.type == goog.events.EventType.MOUSEOUT)) {
    this.mocap(event);
    this.callbackMoved(event);
    var coords = this.coordinates.slice();
    this.terminateMocap();
    this.callbackDone(coords);
  } else if(this.mouseState == this.DOWN &&
            event.type == goog.events.EventType.MOUSEMOVE) {
    this.mocap(event);
    this.callbackMoved(event);
  }
}

ble.mocap.PixelMocap.prototype.initMocap = function(event) {
  this.mouseState = this.DOWN;
  this.startTime = event.getBrowserEvent().timeStamp;
  this.coordinates = [];
}

ble.mocap.PixelMocap.prototype.mocap = function(event) {
  var delta = event.getBrowserEvent().timeStamp - this.startTime;
  this.coordinates.push(delta, event.offsetX, event.offsetY);
}

ble.mocap.PixelMocap.prototype.terminateMocap = function() {
  this.mouseState = this.UP;
  this.startTime = null;
  this.coordinates = null;
}

/**
 * @param {Array.<number>} capture (time, x, y) triplets
 * @param {function(Array.<number>)} callback to call when replaying
 * @constructor
 */

ble.mocap.Replayer = function(capture, callback) {
  this.capture = capture.slice();
  if(capture.length % 3 != 0)
    throw Error('capture must be array of t, x, y triplets');
  this.callback = callback;
  this.timeStart = null;
  this.lastIndex = null;
  this.started = false;
  this.id = null;
}

ble.mocap.Replayer.prototype.delay = 10; //milliseconds

ble.mocap.Replayer.prototype.start = function() {
  if(this.started)
    throw Error('replay already started');
  this.started = true;
  this.timeStart = (new Date()).getTime();
  this.lastIndex = 0;
  this.update = goog.bind(this.update0, this);
  this.id = this.scheduleReplay(this.delay);
}

ble.mocap.Replayer.prototype.scheduleReplay = function(delay) {
  return window.setTimeout(this.update, delay);
}

ble.mocap.Replayer.prototype.update0 = function() {
  var delta = (new Date()).getTime() - this.timeStart;
  var index = this.lastIndex + 1;
  while(index < this.capture.length / 3 && this.capture[index * 3] < delta) 
    index++;
  var coords = [];
  for(var ix = this.lastIndex; ix < index; ix++) {
    coords.push(this.capture[ix * 3 + 1], this.capture[ix * 3 + 2]);
  }
  this.lastIndex = index - 1;
  this.callback(coords);
  if(!(this.lastIndex == this.capture.length / 3 - 1))
    this.id = this.scheduleReplay(this.delay);
  
}




