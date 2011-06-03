
/**
 * @fileoverview Describe and draw curves in terms of their curvature,
 * d(tangent angle) / d(arc length)
 */

goog.provide('ble.curves.Renderer');
goog.provide('ble.curves.Angle');
goog.provide('ble.curves.CurvedPart');
goog.provide('ble.curves');

goog.require('goog.math.Vec2');

ble.curves.DEGREE = Math.PI / 180.0;
ble.curves.ARCMINUTE = ble.curves.DEGREE / 60.0;
ble.curves.ARCSECOND = ble.curves.ARCMINUTE / 60.0;

/**
 * Renders a curvature specification into a sequence of coordinates
 * @param {Array.<ble.curves.CurvaturePart>} curve
 * @param {Array.<number>} origin
 * @param {number} angle0
 * @constructor
 */
ble.curves.Renderer = function(curve, origin, angle0) {
  this.curve = curve.slice();
  this.origin = origin.slice();
  if(origin.length != 2)
    throw Error('bad length of origin coordinate');
  this.angle = angle0;
};

/**
 * @param {number} angle in radians
 * @return {goog.math.Vec2}
 */
ble.curves.tangent = function(angle) {
  return new goog.math.Vec2(Math.cos(angle), Math.sin(angle))
}
ble.curves.Renderer.prototype.deltaAngle = ble.curves.DEGREE;

ble.curves.Renderer.prototype.renderCurve = function() {
  var current = new goog.math.Vec2(this.origin[0], this.origin[1]);
  var coordinates = [];
  coordinates.push(current.x, current.y);

  var angle = this.angle;
  for(var i = 0; i < this.curve.length; i++) {
    var part = this.curve[i];
    var length = part.length;
    var extremeCurvature = Math.abs(part.extremeCurvature);
    var lengthStep = this.deltaAngle / extremeCurvature;
    angle += part.deltaAngle;
    var partLength = 0;
    while(partLength < length) {
      var dLength;
      var evalLength;
      if(length - partLength < lengthStep) {
        dLength = length - partLength;
        evalLength = partLength + dLength / 2;
        partLength = length;
      } else {
        dLength = lengthStep;
        evalLength = partLength + dLength / 2;
        partLength += lengthStep;
      }
      var dAngle = part.curvatureFn(evalLength) * dLength;
      angle += dAngle;
      var tangent = ble.curves.tangent(angle);
      current.add(tangent.scale(dLength));
      coordinates.push(current.x, current.y);
    }
  }
  return coordinates;
}

/**
 * A portion of a curvature-specified curve
 * @constructor
 */
ble.curves.CurvaturePart = function() {};
ble.curves.CurvaturePart.prototype.length = 0;
ble.curves.CurvaturePart.prototype.deltaAngle = 0;
ble.curves.CurvaturePart.prototype.extremeCurvature = 0;
ble.curves.CurvaturePart.prototype.curvatureFn = function(length) { return this.extremeCurvature; }


/**
 * Subsection of a curve where the curvature is constant
 * @param {number} length
 * @constructor
 * @extends {ble.curves.CurvaturePart}
 */
ble.curves.CurvedPart = function(length, curvature) {
  this.extremeCurvature = curvature;
  this.length = length;
  ble.curves.CurvaturePart.call(this);
};
goog.inherits(ble.curves.CurvedPart, ble.curves.CurvaturePart);


/**
 * A sharp angle in a curve
 * @param {number} degrees
 * @constructor
 * @extends {ble.curves.CurvaturePart}
 */
ble.curves.Angle = function(degrees) {
  this.deltaAngle = degrees / ble.curves.DEGREE;
  ble.curves.CurvaturePart.call(this);
};
goog.inherits(ble.curves.Angle, ble.curves.CurvaturePart);

