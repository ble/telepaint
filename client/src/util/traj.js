goog.provide('ble.traj');

goog.require('goog.math.Vec2');
goog.require('goog.math.Size');
goog.require('goog.math.Box');

ble.traj.polar = function(radius, angle) {
  return new goog.math.Vec2(radius, 0).rotate(angle);
}

ble.traj.arc = function(from, fromAngle, radius, toAngle) {
  var center = goog.math.Vec2.difference(from, ble.traj.polar(radius, fromAngle));
  return function(t) {
    var angle = t * (toAngle - fromAngle) + toAngle;
    return goog.math.Vec2.sum(center, ble.traj.polar(radius, angle)); 
  };
};

ble.traj.fixedSizeBoxTraj = function(centerTraj, size) {
  var dx = size.width / 2;
  var dy = size.height / 2;
  return function(t) {
    var c = centerTraj(t);
    return new goog.math.Box(c.y-dy, c.x+dx, c.y+dy, c.x-dx);
  };
};
