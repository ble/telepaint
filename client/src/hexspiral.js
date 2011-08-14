goog.require('goog.math.Vec2');

goog.provide('ble.hexspiral');

ble.hexspiral.tri = function(x) {
  return x * (x + 1) / 2;
};

ble.hexspiral.inverseTri = function(x) {
  var discriminant = 0.25 + 2 * x;
  return Math.sqrt(discriminant) - 0.5;
};

ble.hexspiral.radius = function(ix) {
  if(ix == 0)
    return 0;
  return 1 + Math.floor(ble.hexspiral.inverseTri( (ix - 1) / 6));
};

ble.hexspiral.ringStep = function(ix) {
  if(ix == 0)
    return 0;
  var priorRadius = ble.hexspiral.radius(ix) - 1;
  return ix - (1 + 6 * ble.hexspiral.tri(priorRadius));
};

ble.hexspiral.hexBasisLocation = function(ix) {
  if(ix == 0)
    return [0, 0];
  var radius = ble.hexspiral.radius(ix);
  var step = ble.hexspiral.ringStep(ix);
  var side = Math.floor(step / radius);
  var sideStep = step % radius;
  switch(side) {
    case 0: return [radius, sideStep];
    case 1: return [radius - sideStep, radius];
    case 2: return [-sideStep, radius - sideStep];
    case 3: return [-radius, -sideStep];
    case 4: return [sideStep - radius, -radius];
    case 5: return [sideStep, sideStep - radius]; 
  }
  throw new Error();
};

ble.hexspiral.theta = 2 * Math.PI/3;
var theta = ble.hexspiral.theta;
ble.hexspiral.x = new goog.math.Vec2(1, 0);
ble.hexspiral.y = new goog.math.Vec2(Math.cos(theta), Math.sin(theta));


ble.hexspiral.location = function(ix) {
  var hexLocation = ble.hexspiral.hexBasisLocation(ix);
  var x = ble.hexspiral.x.clone();  
  var y = ble.hexspiral.y.clone();  
  x.scale(hexLocation[0]);
  y.scale(hexLocation[1]);
  return goog.math.Vec2.sum(x, y);
};
