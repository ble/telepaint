
var unit_smiley = function(faceColor) {
  return function(context) {
  var white = "fff";
  var black = "000";

//Multiplying the lineWidth property rather than setting it results in a
//line drawn with a pixel width independent of the subcanvas scaling.
  //context.lineWidth *= 9;
  context.lineWidth = 0.08;
  context.strokeStyle = black;
  context.fillStyle = faceColor;
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
};

var zombor = unit_smiley("6f2");
var normal = unit_smiley("fd2");

var domHelper = new goog.dom.DomHelper();

var body = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(640, 480, 1.0);
canvas.render(body);

var partialView = new goog.math.Box(.375, .5, -.75, -1);
var fullView = new goog.math.Box(1, 1, -1, -1);

var whole = new goog.math.Box(0, 640, 480, 0);
var middling = new goog.math.Box(70, 395, 440, 25);
var small = new goog.math.Box(210, 590, 430, 370);
var tiny = new goog.math.Box(30, 620, 100, 550);

var subcanvasSpec = [[whole, partialView], [middling, fullView], [small, fullView], [tiny, fullView]];
var subcanvases = goog.array.map( subcanvasSpec, function( spec ) {
  return new ble.scratch.Subcanvas(canvas, spec[0], spec[1]);
});

var eventTypes =
  [goog.events.EventType.CLICK,
   goog.events.EventType.MOUSEDOWN,
   goog.events.EventType.MOUSEMOVE];

canvas.forwardEvents(eventTypes);

var zombieDot = function(e) {
  this.withContext(zombor);
  var virtualCoords = [e.virtualX, e.virtualY];
  this.withContext(function(context) {
    context.fillStyle = "f00";
    context.beginPath();
    context.arc(e.virtualX, e.virtualY, 0.05, 0.0, 2.0 * Math.PI);
    context.fill();
  });
  return false;
};

var isDown = false;
var toggler = function(e) {
  if(e.type === goog.events.EventType.MOUSEDOWN)
    isDown = true;
  else if(e.type === goog.events.EventType.MOUSEUP)
    isDown = false;
}

var zombieDotWhenDown = function(e) {
  if(isDown) {
    return zombieDot.call(this, e);
  }
}

goog.events.listen(canvas.element_, [goog.events.EventType.MOUSEDOWN, goog.events.EventType.MOUSEUP], toggler);
goog.array.forEach( subcanvases, function( subcanvas ) {
  subcanvas.withContext(normal);
  goog.events.listen(subcanvas, [goog.events.EventType.MOUSEDOWN, goog.events.EventType.MOUSEUP], toggler);
  goog.events.listen(subcanvas, [goog.events.EventType.MOUSEDOWN, goog.events.EventType.MOUSEMOVE], zombieDotWhenDown, false, subcanvas);
/*  goog.events.listen(subcanvas, goog.events.EventType.CLICK, function(e) {
    this.withContext(zombor);
    console.log(e);
    this.withContext(function(context) {
      context.fillStyle = "f00";
      context.beginPath();
      context.arc(e.virtualX, e.virtualY, 0.05, 0.0, 2.0 * Math.PI);
      context.fill();
    });
    return false;
  }, false, subcanvas);*/
  canvas.addSubcanvas(subcanvas);
});

