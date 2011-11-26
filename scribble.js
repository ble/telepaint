goog.require('ble.interval.startRank');
goog.require('ble.util.comparatorFromRank');
goog.provide('ble.scribble.Drawing');
goog.provide('ble.scribble.MutableDrawing');

/**
 * @constructor
 * @param {Array.<ble._2d.DrawPart>} items
 * @implements {ble._2d.DrawPart}
 */
ble.scribble.Drawing = function(startTime, items) {
  this.fetcher = new ble.interval.Fetcher(items);
  this.byStart = items.slice();
  this.byStart.sort(this.compareStart_);
  this.startTime = startTime;
};

ble.scribble.Drawing.prototype.compareStart_ =
  ble.util.comparatorFromRank(ble.interval.startRank);

ble.scribble.Drawing.prototype.start = function() {
  return this.startTime;
};

ble.scribble.Drawing.prototype.end = function() {
  return this.start() + this.length();
};

ble.scribble.Drawing.prototype.length = function() {
  return this.fetcher.byEnd[this.fetcher.byEnd.length - 1].end();
};

ble.scribble.Drawing.prototype.withStartAt = function(newTime) {
  return new ble.scribble.Drawing(newTime, this.byStart);
};

ble.scribble.Drawing.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var scale = function(i0) {
    var i1 = i0.withStartAt(i0.start() * scaleFactor);
    var i2 = i1.withLength(i1.length() * scaleFactor);
    return i2;
  };
  var byStart = this.byStart.map(scale);
  return new ble.scribble.Drawing(this.startTime, byStart);
};

ble.scribble.Drawing.prototype.draw = function(ctx) {
  for(var i = 0; i < this.byStart.length; i++) {
    this.byStart[i].draw(ctx);
  }
};

ble.scribble.Drawing.prototype.at = function(time) {
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

/**
 * @constructor
 * @param {Array.<ble._2d.DrawPart>} items
 * @extends {ble.scribble.Drawing}
 */
ble.scribble.MutableDrawing = function(startTime, items) {
  ble.scribble.Drawing.call(this, startTime, items);
  this.current = null;
};
goog.inherits(ble.scribble.MutableDrawing, ble.scribble.Drawing);

/**
 * @override
 */
ble.scribble.MutableDrawing.prototype.draw = function(ctx) {
  ble.scribble.Drawing.prototype.draw.call(this, ctx);
  this.current.draw(ctx);
};

/**
 * @param {ble._2d.DrawPart} item
 */
ble.scribble.MutableDrawing.prototype.add = function(item) {
  var item1 = item.withStartAt(item.start() - this.start());
  this.fetcher.add(item1);
  ble.util.rankBinaryInsert(ble.interval.startRank, this.byStart, item1);
};

/**
 * @param {ble._2d.DrawPart} item
 */
ble.scribble.MutableDrawing.prototype.setCurrent = function(item) {
  this.current = item;
};

ble.scribble.MutableDrawing.prototype.recordCurrent = function() {
  this.add(this.current);
  this.current = null;
};


