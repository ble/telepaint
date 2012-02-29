goog.require('goog.net.XhrIo');
goog.require('goog.events.EventTarget');
goog.require('goog.events.Event');

goog.require('goog.Uri');
goog.require('goog.Uri.QueryData');

goog.require('goog.functions');

goog.require('ble.json.RpcCall');

goog.provide('ble.comet.Basic');
goog.provide('ble.comet.Queue');
goog.provide('ble.comet.Queue.Update');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
var console = window.console;
//Basic comet loop:
//  hit a fixed URL with get requests, again and again.
/**
 * @constructor
 * @extends {goog.events.EventTarget}
 * @param {string} uri
 * @param {number} timeoutMillis
 * @param {number} successWaitMillis
 * @param {number} retryWaitMillis
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
  var uri = this.getUri();
  this.preSend(uri);
  goog.events.listenOnce(this.xhr, this.xhrEventTypes, this);
  this.xhr.send(uri);
};

/**
 * @protected
 */
bcBp.preSend = function(uri) {};

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
      this.processSuccess(event);
      var delay = this.successWait;
      this.pendingSend = window.setTimeout(goog.bind(this.send_, this), delay);
      break;
    default:
      console.error('Unexpected event type:' + event.type);
  }
};

bcBp.processSuccess = function(event) {
  this.dispatchEvent(event);
};

bcBp.disposeInternal = function() {
  this.xhr.dispose();
  ble.comet.Basic.superClass_.disposeInternal.call(this);
};

//Queue comet loop:
//Keep on hitting up a URL with a query string dependent on the last time
//reported to us by the server; get responses with a new reported time, use
//that to update the reported time.
/**
 * @constructor
 * @extends {ble.comet.Basic}
 * @param {string} uri
 * @param {Array.<number>} lastObservedTime
 */
ble.comet.Queue =
 function(
     uri,
     lastObservedTime) {
  ble.comet.Basic.call(this, uri, 10000, 1000, 5000);
  this.lastObservedTime = lastObservedTime;
};
goog.inherits(ble.comet.Queue, ble.comet.Basic);

var bcQp = ble.comet.Queue.prototype;

bcQp.getUri = function() {
  var query = new goog.Uri.QueryData();
  query.add('mega', this.lastObservedTime[0]);
  query.add('unit', this.lastObservedTime[1]);
  query.add('micro', this.lastObservedTime[2]);
  var uri = new goog.Uri(this.uri);
  uri.setQueryData(query);
  return uri.toString();
};

bcQp.preSend = function(uri) {
  console.log('about to request ' + uri);
};

bcQp.processSuccess = function(event) {
  /** @type {goog.net.XhrIo} */
  var xhr = event.target;

  var json = ble.json.RpcResponse.coerce(xhr.getResponseJson());

  var when = json['result']['when'];

  if(when.length != 3)
    throw new Error('bad response from queue');

  this.dispatchEvent(new ble.comet.Queue.Update(this, json, when));
  this.lastObservedTime = when;

};

/**
 * @constructor
 * @extends {goog.events.Event}
 */
ble.comet.Queue.Update = function(target, json, when) {
  goog.events.Event.call(this, ble.comet.Queue.Update.EventType, when);
  this.json = json;
  this.when = when;
};
ble.comet.Queue.Update.EventType = 'UPDATE';
var updateType = ble.comet.Queue.Update.EventType;


bcQp.dispatchedEventTypes = [types.ERROR, types.ABORT, types.TIMEOUT, updateType, 'STARTED', 'STOPPED'];
////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
