goog.provide('ble.net.ChunkedChannel');
goog.provide('ble.net.ChannelEvent');
goog.provide('ble.net.EventType');


goog.require('goog.events.EventTarget');
goog.require('goog.events.EventType');

/**
 * @enum {string}
 */
ble.net.EventType = {
  DATA: 'data'
};

/**
 * @constructors
 * @extends {goog.events.Event}
 */
ble.net.ChannelEvent = function(src, data) {
  goog.events.Event.call(this, ble.net.EventType.DATA, src);
  this.data = data;
};

/**
 *
 * @constructor
 * @extends {goog.events.EventTarget}
 */

ble.net.ChunkedChannel = function() {
  goog.events.EventTarget.call(this);
  this.lastLength = null;
  this.xhr = null;
};
goog.inherits(ble.net.ChunkedChannel, goog.events.EventTarget);

ble.net.ChunkedChannel.prototype.open = function(method, url) {
  if(this.xhr == null) {
    this.xhr = new XMLHttpRequest();
    this.xhr.onreadystatechange = goog.bind(this.handler, this);
  }
  this.xhr.open(method, url);
  this.lastLength = 0;
};

ble.net.ChunkedChannel.prototype.send = function() {
  this.xhr.send();
};

ble.net.ChunkedChannel.prototype.header = "[{\"message_start\":null},";
ble.net.ChunkedChannel.prototype.footer = ",{\"message_end\":null}]";

ble.net.ChunkedChannel.prototype.extractSegments = function(dataString) {
  var begin = 0;
  var header = this.header;
  var footer = this.header;
  var begin = 0;
  var segments = [];
  do {
    var start = dataString.indexOf(header, begin);
    if(start == -1) break;
    start += header.length;
    var end = dataString.indexOf(footer, begin);
    if(end == -1) break;
    segments.push(dataString.substring(start, end));
    begin = end + footer.length; 
  } while(true);
  var result = {end: begin, segments: segments};
  return result;
};

ble.net.ChunkedChannel.prototype.handler = function() {
  if(this.xhr.readyState == 3 || this.xhr.readyState == 4) {
    var fullResp = this.xhr.responseText;
    var newPart = fullResp.substring(this.lastLength, fullResp.length);
    var res = this.extractSegments(newPart);

    this.lastLength = res.end;

    var objs = goog.array.map(res.segments, goog.json.parse);
    this.dispatchEvent(new ble.net.ChannelEvent(this, objs));
  }
};

/**
 * Oh the secret shame.
 */

