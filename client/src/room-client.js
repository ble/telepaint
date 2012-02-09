
goog.require('goog.net.Cookies');
goog.require('goog.net.XhrIo');
goog.require('ble.hate');

goog.provide('ble.room.Client');
var console = window.console;

/**
 * @constructor
 */
ble.room.Client = function() {
  var pattern = /^http:\/\/([^\/]*)\/room_client\/([0-9a-zA-Z_-]+)$/;
  var match = window.location.href.match(pattern);
  if(!match)
    throw new Error('unexpected location');
  this.host = match[1];
  this.roomId = match[2];
  var c = new goog.net.Cookies(document);
  this.observerId = c.get('observerId');
  console.log(this);


};

ble.room.Client.prototype.setupLinks = function() {
  var obj = (
      {'room': '/room/' + this.roomId,
       'queue': '/queue/' + this.observerId});
  ble.hate.addLinks(obj);
};

var client = new ble.room.Client();
client.setupLinks();


