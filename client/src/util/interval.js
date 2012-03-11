goog.require('ble.util.isSortedBy');
goog.require('ble.util.comparatorFromRank');

goog.provide('ble.interval');
goog.provide('ble.interval.Interval');
goog.provide('ble.interval.AdjustableInterval');
goog.provide('ble.interval.Impl');

goog.provide('ble.interval.Tweaker');
goog.provide('ble.interval.NoOverlapTweaker');
goog.provide('ble.interval.Fetcher');

goog.provide('ble.interval.startRank');
goog.provide('ble.interval.endRank');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////
var _ = ble.interval;
/**
 * @interface
 */
_.Interval = function() {};

/**
 * @return {number}
 */
_.Interval.prototype.start = function() {};

/**
 * @return {number}
 */
_.Interval.prototype.end = function() {};

/**
 * @return {number}
 */
_.Interval.prototype.length = function() {};

/**
 * @interface
 * @extends {ble.interval.Interval}
 */
_.AdjustableInterval = function() {};

/**
 * @param {number} newStart
 * @return {ble.interval.AdjustableInterval}
 */
_.AdjustableInterval.prototype.withStartAt = function(newStart) {};

/**
 * @param {number} newLength
 * @return {ble.interval.AdjustableInterval}
 */
_.AdjustableInterval.prototype.withLength = function(newLength) {};

/**
 * @constructor
 * @implements {ble.interval.AdjustableInterval}
 */
_.Impl = function(start, end) {
  this.start_ = start;
  this.end_ = end;
};

_.Impl.prototype.start = function() { return this.start_; };

_.Impl.prototype.end = function() { return this.end_; };

_.Impl.prototype.length = function() { return this.end_ - this.start_; };

/**
 * @override
 * @return {ble.interval.Impl}
 */
_.Impl.prototype.withStartAt = function(newStart) {
  return new _.Impl(newStart, newStart + this.length());
};

/**
 * @override
 * @return {ble.interval.Impl}
 */
_.Impl.prototype.withLength = function(newLength) {
  return new _.Impl(this.start(), this.start() + newLength);
};

_.union = function(x, y) {
  return new _.Impl(
      Math.min(x.start(), y.start()),
      Math.max(x.end(), y.end()));
};

_.overlaps = function(x, y) {
  var xS = x.start(), xE = x.end();
  var yS = y.start(), yE = y.end();
         //one or both of x's endpoints are in y
  return (xS <= yE && xS >= yS) || 
         (xE <= yE && xE >= yS) ||
         //if x and y overlap but neither of x's endpoints are in y, then
         //y must be entirely contained in x.
         (xS < yS && xE > yE);
};

_.withoutGapsFromZero = function(startSortedIntervals) {
  var intervals = startSortedIntervals.slice();
  var intervalsOut = [];
  var end = 0;

//%if we just wanted no gaps and didn't care about starting from zero...
//  if(intervals.length > 0)
//    end = intervals[0].start;

  while(intervals.length > 0) {

    //Get the next interval, align it without a gap
    var itemIn = intervals.shift();
    intervalsOut.push(itemIn.withStartAt(end));

    var span = itemIn;
    var spanStart = span.start();

    while(intervals.length > 0 && _.overlaps(span, intervals[0])) {
      var overlappedItem = intervals.shift();

      //Grow the interval covered by the overlapping intervals
      span = _.union(span, overlappedItem);

      //Set the start to the correct offset
      var newStart = end + overlappedItem.start() - spanStart;
      intervalsOut.push(overlappedItem.withStartAt(newStart));
    }

    //The next interval will begin where the set of overlapping intervals
    //ended.
    end += span.length();
  }
  return intervalsOut;

};

_.shiftBy = function(interval, amount) {
  interval.withStartAt(interval.start() + amount);
};

_.startRank = function(x) { return x.start(); };
_.endRank = function(x) { return x.end(); };

/**
 * @constructor
 * @param {Array.<ble.interval.Interval>} data
 */
_.Fetcher = function(data) {
  this.byEnd = data.slice();
  this.byEnd.sort(this.comparator_);
};

/**
 * @const
 */
_.Fetcher.prototype.comparator_ =
  ble.util.comparatorFromRank(_.endRank);
/**
 * @const
 */
_.Fetcher.prototype.rank_ = _.endRank;


/**
 * @param {number} point
 * @return {Array.<Array.<ble.interval.Interval>>}
 */
_.Fetcher.prototype.beforeAndDuring = function(point) {
  var n = this.byEnd.length;
  var lastBefore =
    Math.ceil(ble.util.rankBinarySearch(this.rank_, this.byEnd, point));
  while(lastBefore < n && this.byEnd[lastBefore].end() <= point)
    lastBefore++;
  var before = this.byEnd.slice(0, lastBefore);
  var after = this.byEnd.slice(lastBefore);
  var during = after.filter(function(interval) { return interval.start() < point; });
  return [before, during]; 
};

/**
 * @param {ble.interval.Interval} interval
 */
_.Fetcher.prototype.add = function(interval) {
  ble.util.rankBinaryInsert(this.rank_, this.byEnd, interval);
};

_.makeRandom = function(samples, size) {
  var result = [];
  for(var i = 0; i < samples; i++) {
    var a = Math.round(Math.random() * size),
        b = Math.round(Math.random() * size),
        start = Math.min(a, b),
        end = Math.max(Math.max(a, b), start + 1);
    result.push(new _.Impl(start, end)); 
  }
  return result;
}; 

////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
