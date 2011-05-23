



var domHelper = new goog.dom.DomHelper();

var body = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(640, 480, 1.0);
canvas.render(body);

var unit_smiley = function(context) {
  var happy_yellow = "fd2";
  var white = "fff";
  var black = "000";

//Multiplying the lineWidth property rather than setting it results in a
//line drawn with a pixel width independent of the subcanvas scaling.
  //context.lineWidth *= 9;
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


var subcanvas_whole = new ble.scratch.Subcanvas(canvas, [0, 0, 640, 480], [-1, .375, .5, -.75]);
var subcanvas_virtual = new ble.scratch.Subcanvas(canvas, [25, 70, 395, 440], [-1, 1, 1, -1]);
var subcanvas_small = new ble.scratch.Subcanvas(canvas, [370, 210, 585, 425], [-1, 1, 1, -1]);

subcanvas_whole.withContext(unit_smiley);
subcanvas_virtual.withContext(unit_smiley);
subcanvas_small.withContext(unit_smiley);
