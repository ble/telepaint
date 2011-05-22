



var domHelper = new goog.dom.DomHelper();

var body = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(640, 480, 1.0);
canvas.render(body);
/*
canvas.withContext( function(context) {
  context.beginPath();
  context.moveTo(50, 50);
  context.lineTo(100, 0);
  context.strokeStyle = "000";
  context.stroke();
});*/


var unit_smiley = function(context) {
  var happy_yellow = "fd2";
  var white = "fff";
  var black = "000";

//  context.lineWidth *= 9;
  context.lineWidth = 0.08;
  context.strokeStyle = black;
  context.fillStyle = happy_yellow;
  context.beginPath();
  context.arc(0, 0, .95, 0.0, 2.0 * Math.PI);
  context.fill();
  context.stroke();


  var eye = function(context) {
    context.strokeStyle = black;
    context.fillStyle = white;
    context.beginPath();
    context.arc(0, 0, 0.25, 0.0, 2.0 * Math.PI);
    context.fill();
    context.stroke();
  };
  context.save(); {
    context.translate(0.375, 0.375);
    eye(context);
  } context.restore();

  context.save(); {
    context.translate(-0.375, 0.375);
    eye(context);
  } context.restore();

  context.fillStyle = white;
  context.beginPath();
  context.arc(0, -.1, .65, Math.PI, 2.0 * Math.PI);
  context.closePath();
  context.fill();
  context.stroke();

  context.save(); {
    context.clip();
    context.lineWidth /= 2;
    var toothSpacing = 0.125;
    for(var x = -1.0; x <= 1.0; x += toothSpacing) {
      context.beginPath();
      context.moveTo(x, 1.0);
      context.lineTo(x, -1.0);
      context.stroke();
    }
    context.beginPath();
    context.moveTo(-1, -.4);
    context.lineTo(1, -.4);
    context.stroke();
  } context.restore();
  
};

var blueBox = function(context) {
  var xs = [this.virtualCoords_[0], this.virtualCoords_[2]];
  var ys = [this.virtualCoords_[1], this.virtualCoords_[3]];
  context.beginPath();
  context.moveTo(xs[0], ys[0]);
  context.lineTo(xs[1], ys[0]);
  context.lineTo(xs[1], ys[1]);
  context.lineTo(xs[0], ys[1]);
  context.closePath();
  context.strokeStyle = "0ff";
  context.fillStyle = "8ff";
  context.lineWidth = 10;
  context.stroke();
  context.fill();
};

var subcanvas_whole = new ble.scratch.Subcanvas(canvas, [0, 0, 640, 480], [-1, .375, .5, -.75]);
var subcanvas_pixels = new ble.scratch.Subcanvas(canvas, [50, 50, 590, 430]);
var subcanvas_virtual = new ble.scratch.Subcanvas(canvas, [25, 70, 395, 440], [-1, 1, 1, -1]);
var subcanvas_small = new ble.scratch.Subcanvas(canvas, [370, 210, 585, 425], [-1, 1, 1, -1]);

subcanvas_whole.withContext(unit_smiley);
//subcanvas_pixels.withContext(blueBox);
subcanvas_virtual.withContext(unit_smiley);
subcanvas_small.withContext(unit_smiley);
