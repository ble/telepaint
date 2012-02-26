goog.require('goog.net.XhrIo');
goog.require('goog.events.EventTarget');
goog.require('goog.events.Event');
goog.require('goog.functions');

goog.provide('ble.comet.Basic');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////

//Basic comet loop:
//  hit a fixed URL with get requests, again and again.
/**
 * @constructor
 * @extends {goog.events.EventTarget}
 * @param {string} uri
 * @param {number} timeoutMillis
 * @param {number} successWaitMillis
 * @param {function(number): number} retryWaitMillis
 */ 
ble.comet.Basic =
 function(
   uri,
   timeoutMillis,
   successWaitMillis,
   retryWaitMillis) {
  this.uri = uri
  this.timeout = timeoutMillis;
  this.successWait = successWaitMillis;
  this.retryWait = goog.functions.constant(retryWaitMillis);

  this.previousFailures = 0;
  this.running = false;
  this.xhr = new goog.net.XhrIo();
  this.xhr.setTimeoutInterval(this.timeout);
  this.pendingSend = null;
};
goog.inherits(ble.comet.Basic, goog.events.EventTarget);

var bcBp = ble.comet.Basic.prototype;

bcBp.run = function() {
  if(this.running)
    return;
  this.running = true;
  this.send_();
  this.dispatchEvent('STARTED');
};

bcBp.stop = function() {
  if(!this.running)
    return;
  this.xhr.abort();
  this.running = false;
  if(this.pendingSend !== null)
    window.clearTimeout(this.pendingSend);
  this.pendingSend = null;
  this.previousFailures = 0;
  this.dispatchEvent('STOPPED');

};
/**
 * @protected
 */
bcBp.getUri = function() {
  return this.uri;
};

var types = goog.net.EventType;
/**
 * @protected
 */
bcBp.xhrEventTypes = [types.ERROR, types.ABORT, types.TIMEOUT, types.SUCCESS];
bcBp.dispatchedEventTypes = [types.ERROR, types.ABORT, types.TIMEOUT, types.SUCCESS, 'STARTED', 'STOPPED'];

/**
 * @private
 */
bcBp.send_ = function() {
  this.pendingSend = null;
  this.xhr.send(this.getUri());
  goog.events.listenOnce(this.xhr, this.xhrEventTypes, this);
};

/**
 * @param {goog.events.Event} event
 */
bcBp.handleEvent = function(event) {
  switch (event.type) {
    case types.ERROR:
    case types.TIMEOUT:
      var delay = this.retryWait(this.previousFailures);
      this.previousFailures++;
      this.pendingSend = window.setTimeout(goog.bind(this.send_, this), delay);
      this.dispatchEvent(event);
      break;
    case types.ABORT:
      break;
    case types.SUCCESS:
      this.dispatchEvent(event);
      var delay = this.successWait;
      this.pendingSend = window.setTimeout(goog.bind(this.send_, this), delay);
      break;
  }
};

bcBp.disposeInternal = function() {
  this.xhr.dispose();
  ble.comet.Basic.superClass_.disposeInternal.call(this);
};

////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
