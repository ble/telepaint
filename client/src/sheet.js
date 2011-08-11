goog.require('goog.events.EventTarget');
goog.require('goog.events.Event');

goog.provide('ble.sheet.Client');
goog.provide('ble.sheet.EventType');

/**
 * @enum {string}
 */
ble.sheet.EventType = {
  SUCCESS: 'success',
  FETCH: 'fetch'
};

/**
 * @constructor
 * @extends{goog.events.EventTarget}
 */
ble.sheet.Client = function(url) {
  goog.events.EventTarget.call(this);
  this.url = url;
}

goog.inherits(ble.sheet.Client, goog.events.EventTarget);

var JSON;
var console;

ble.sheet.Client.prototype.sheetAppend = function(method, data) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', this.url);
  var rpc = ({'method': method, 'data': data});
  var onChange = goog.bind(function() {
    console.log(xhr.readyState);
    if(xhr.readyState == 4) {
      console.log({'status': xhr.status, 'response': xhr.responseText});
      console.log('append status ' + xhr.status);
    }
  }, this);
  xhr.onreadystatechange = onChange;
  xhr.send(JSON.stringify(rpc));
};

ble.sheet.Client.prototype.read = function() {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', this.url);
  var onChange = goog.bind(function() {
    console.log(xhr.readyState);
    if(xhr.readyState == 4) {
      console.log('status ' + xhr.status);
      if(xhr.status == 200) {
        var e = new goog.events.Event(ble.sheet.EventType.FETCH);
        console.log(xhr);
        var response = JSON.parse(xhr.responseText);
        e.fragments = response.fragments;
        this.dispatchEvent(e); 
      } 
    }
  }, this);
  xhr.onreadystatechange = onChange;
  xhr.send(); 
};
