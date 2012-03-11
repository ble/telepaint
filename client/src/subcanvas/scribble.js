goog.require('ble.interval.startRank');
goog.require('ble.util.comparatorFromRank');
goog.require('ble.interval');
goog.require('ble._2d');

goog.provide('ble.scribble.Drawing');
goog.provide('ble.scribble.SmartDrawing');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
var _ = ble.scribble;
/**
 * @constructor
 * @param {Array.<ble._2d.DrawPart>} items
 * @implements {ble._2d.DrawPart}
 */
_.Drawing = function(startTime, items) {
  this.fetcher = new ble.interval.Fetcher(items);
  this.byStart = items.slice();
  this.byStart.sort(this.compareStart_);
  this.startTime = startTime;
};

_.Drawing.prototype.compareStart_ =
  ble.util.comparatorFromRank(ble.interval.startRank);

_.Drawing.prototype.start = function() {
  return this.startTime;
};

_.Drawing.prototype.end = function() {
  return this.start() + this.length();
};

_.Drawing.prototype.length = function() {
  var byEnd = this.fetcher.byEnd;
  if(byEnd.length == 0)
    return 0;
  
  return byEnd[byEnd.length - 1].end();
};

_.Drawing.prototype.withStartAt = function(newTime) {
  return new _.Drawing(newTime, this.byStart);
};

_.Drawing.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var scale = function(i0) {
    var i1 = i0.withStartAt(i0.start() * scaleFactor);
    var i2 = i1.withLength(i1.length() * scaleFactor);
    return i2;
  };
  var byStart = this.byStart.map(scale);
  return new _.Drawing(this.startTime, byStart);
};

_.Drawing.prototype.draw = function(ctx) {
  for(var i = 0; i < this.byStart.length; i++) {
    this.byStart[i].draw(ctx);
  }
};

_.Drawing.prototype.at = function(time) {
  time -= this.start();
  if(time < 0)
    return ble._2d.nothing;
  if(time >= this.length())
    return this;

  //Assembling all of the right Drawables in the right order is a little 
  //tricky; they must all be sorted according to the start of their interval,
  //but the DrawParts that are only partially complete must be converted into
  //Drawables which do not have any interval information.

  //The approach used is to add a (hopefully/almost certainly unique) field to
  //the DrawParts which must be converted, put all DrawParts into one array
  //and sort by start, then replace each flagged item in the array with the
  //partially complete item and remove the flag from the Drawable.

  //There are more elegant ways of doing this, primarily by having the partial
  //Drawables carry the interval information.  The elegant way of doing that
  //is to set the prototype of a newly-created partial Drawable to be the
  //instance from which it was created.  Unfortunately, neither __proto__ nor
  //Object.create() are guaranteed to work on all browsers of interest, and I
  //find the
  /*
     function inherit(proto) {
      function F() {}
      F.prototype = proto
      return new F();
    };
  */
  //approach intriguing but uncertain.  It'll be nice when one can assume
  //that Object.create() is present on any browser; prototypal OO is much less
  //compelling when you can't make an object's prototype an instance rather
  //than a constructor.

  var paired = this.fetcher.beforeAndDuring(time);
  var during = paired[1];

  //Flag all of the items which will be partially drawn
  during.map(function(item) { item.__duringFlag__ = true; });

  //Concatenate and sort all the items
  var toDraw = paired[0].concat(during);
  toDraw.sort(this.compareStart_);

  //Replace all the items in the array which should be partially drawn with
  //their partial forms and unflag the original items.
  goog.array.forEach(
    toDraw,
    function(value, index, array) {
      if(value.__duringFlag__) {
        array[index] = value.at(time);
        delete(value.__duringFlag__);
      }
    });

  var draw = function(items, N, ctx) {
    for(var i = 0; i < N; i++) {
      items[i].draw(ctx);
    }
  };
  var drawable = new ble._2d.Nothing();
  drawable.draw = goog.partial(draw, toDraw, toDraw.length); 
  return drawable;
};

var _ = ble.scribble;

var Also = ble._2d.Also;

var withoutGaps = ble.interval.withoutGapsFromZero;
var overlaps = ble.interval.overlaps;
var shiftBy = ble.interval.shiftBy;
var nothing = ble._2d.nothing;
var startRank = ble.interval.startRank;
var rankBinaryInsert = ble.util.rankBinaryInsert;
/**
 * @constructor
 * @extends {ble.scribble.Drawing}
 *
 * A mutable drawing which maintains the true times of all of its strokes
 * as well as a compact drawing in which the first stroke starts at zero
 * and time gaps between strokes have been eliminated.
 */
_.SmartDrawing = function(startTime, items) {
  _.Drawing.call(this, startTime, items);
  this.updated_ = true;
  this.currentPart = nothing;
};
goog.inherits(_.SmartDrawing, _.Drawing);

_.SmartDrawing.prototype.compact = function() {
  if(this.updated_) { 
    this.compacted = new _.CompactDrawing(withoutGaps(this.byStart), this);
    this.updated_ = false;
  }
  return this.compacted;
};

_.SmartDrawing.prototype.add = function(item) {
  this.fetcher.add(item);
  rankBinaryInsert(startRank, this.byStart, item);
  this.updated_ = true;
};

_.SmartDrawing.prototype.setCurrent = function(item) {
  if(!goog.isDefAndNotNull(item))
    item = nothing;
  this.currentPart = item;
};

_.SmartDrawing.prototype.getCurrent = function() {
  return this.currentPart;
};

_.SmartDrawing.prototype.recordCurrent = function() {
  if(this.getCurrent() !== nothing) {
    this.add(this.getCurrent());
    this.setCurrent(null);
  }
};

_.SmartDrawing.prototype.draw = function(ctx) {
  goog.base(this, 'draw', ctx);
  this.getCurrent().draw(ctx);
};

_.SmartDrawing.prototype.at = function(time) {
  return new Also(goog.base(this, 'at', time), this.getCurrent());
};


_.CompactDrawing = function(items, smart) {
  _.Drawing.call(this, 0, items);
  this.smart = smart;
};
goog.inherits(_.CompactDrawing, _.Drawing);

_.CompactDrawing.prototype.getCurrent = function() {
  return this.smart.getCurrent();
};

_.CompactDrawing.prototype.draw = _.SmartDrawing.prototype.draw; 
_.CompactDrawing.prototype.at = _.SmartDrawing.prototype.at;
////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
