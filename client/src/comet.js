

goog.require('goog.events.Event');
goog.require('goog.events.EventTarget');
goog.require('goog.net.XhrIo');


goog.provide('ble.net.MACometLoop');

/**
 * Comet loop for receiving time-stamped messages from a kinda specific
 * comet resource.
 *
 * @constructor
 * @extends {goog.events.EventTarget}
 */

ble.net.MACometLoop = function(baseUrl, timeout, lastFetched) {
  goog.base(this);
  this.baseUrl = baseUrl; 
  this.timeout = timeout;
  if(!goog.isDef(lastFetched)) {
    lastFetched = [0, 0, 0];
  } else {
    lastFetched = lastFetched.slice(0, 3);
  }
  this.lastFetched = lastFetched;
  this.running = false; 
  this.xhr = null;
  this.lastClientTime = null;
};
goog.inherits(ble.net.MACometLoop, goog.events.EventTarget);


ble.net.MACometLoop.prototype.successType = goog.net.EventType.COMPLETE;
ble.net.MACometLoop.prototype.errorType = goog.net.EventType.ERROR;

ble.net.MACometLoop.prototype.url = function() {
  var L = this.lastFetched;
  return this.baseUrl + L[0] + "/" + L[1] + "/" + L[2];
};

ble.net.MACometLoop.prototype.handleEvent = function(e) {
  if(this.xhr.isSuccess()) {
    this.lastClientTime = Date.now();
    var json = this.xhr.getResponseJson();
    var event = new goog.events.Event(this.successType);
    if(json == "no_data") {
      event.data = null;
    } else {
      this.lastFetched = json.lastFetched;
      event.data = json.data;
    }
    this.dispatchEvent(event);
    this.getAgain_();
  } else {
    var errorEvent = new goog.events.Event(this.errorType);
    this.dispatchEvent(errorEvent);
  }
};

ble.net.MACometLoop.prototype.start = function() {
  if(this.running)
    return;
  this.xhr = new goog.net.XhrIo();
  this.xhr.setTimeoutInterval(this.timeout);
  goog.events.listen(this.xhr, goog.net.EventType.COMPLETE, this);
  this.running = true;
  this.getAgain_();
};

ble.net.MACometLoop.prototype.getAgain_ = function() {
  this.xhr.send(this.url(), 'GET');
};
