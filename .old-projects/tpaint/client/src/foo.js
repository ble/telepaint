
goog.require('goog.dom.DomHelper');
goog.require('goog.events');
goog.require('goog.net.EventType');
goog.require('goog.net.ImageLoader');

goog.require('ble.scratch.Canvas');

goog.provide('ble.foo.run');

var console;

ble.foo.run = function() {
  var domHelper = new goog.dom.DomHelper();
  var container = domHelper.getElement("outermost");

  var width = 800;
  var aspect = 0.75;
  var height = aspect * width;

  var canvas = new ble.scratch.Canvas(width, height);
  canvas.render(container);
  var loader = new goog.net.ImageLoader(container);

  var imgUrl = "/static-compiled/img/cecil_dark_portrait.gif";
  var imgTag = domHelper.createDom("img", {'src': imgUrl}, null);
  var imgId = "my_image"
  loader.addImage(imgId, imgTag);

  goog.events.listen(loader, goog.net.EventType.COMPLETE, function(e) {
    console.log("done");
    canvas.withContext(function(ctx) {
      try {
      ctx.drawImage(imgTag, 0, 0, imgTag.width * 4, imgTag.height * 4);
      } catch(error) {
        console.log(error);
      }
    });
  });
  loader.start();

};
