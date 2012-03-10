goog.require('ble.util.isSortedBy');
goog.require('ble.util.comparatorFromRank');

goog.provide('ble.interval.Interval');
goog.provide('ble.interval.AdjustableInterval');
goog.provide('ble.interval.Impl');

goog.provide('ble.interval.Tweaker');
goog.provide('ble.interval.NoOverlapTweaker');
goog.provide('ble.interval.Fetcher');

goog.provide('ble.interval.startRank');
goog.provide('ble.interval.endRank');

/**
 * @interface
 */
ble.interval.Interval = function() {};

/**
 * @return {number}
 */
ble.interval.Interval.prototype.start = function() {};

/**
 * @return {number}
 */
ble.interval.Interval.prototype.end = function() {};

/**
 * @return {number}
 */
ble.interval.Interval.prototype.length = function() {};

/**
 * @interface
 * @extends {ble.interval.Interval}
 */
ble.interval.AdjustableInterval = function() {};

/**
 * @param {number} newStart
 * @return {ble.interval.AdjustableInterval}
 */
ble.interval.AdjustableInterval.prototype.withStartAt = function(newStart) {};

/**
 * @param {number} newLength
 * @return {ble.interval.AdjustableInterval}
 */
ble.interval.AdjustableInterval.prototype.withLength = function(newLength) {};

/**
 * @constructor
 * @implements {ble.interval.AdjustableInterval}
 */
ble.interval.Impl = function(start, end) {
  this.start_ = start;
  this.end_ = end;
};

ble.interval.Impl.prototype.start = function() { return this.start_; };

ble.interval.Impl.prototype.end = function() { return this.end_; };

ble.interval.Impl.prototype.length = function() { return this.end_ - this.start_; };

/**
 * @override
 * @return {ble.interval.Impl}
 */
ble.interval.Impl.prototype.withStartAt = function(newStart) {
  return new ble.interval.Impl(newStart, newStart + this.length());
};

/**
 * @override
 * @return {ble.interval.Impl}
 */
ble.interval.Impl.prototype.withLength = function(newLength) {
  return new ble.interval.Impl(this.start(), this.start() + newLength);
};


ble.interval.startRank = function(x) { return x.start(); };
ble.interval.endRank = function(x) { return x.end(); };

/**
 * @constructor
 * @abstract
 */
ble.interval.Tweaker = function() {};

/**
 * @param {Array.<ble.interval.AdjustableInterval>} intervals
 * @return {Array.<ble.interval.AdjustableInterval>}
 */
ble.interval.Tweaker.prototype.tweakedIntervals = function(intervals) {
  var plan = this.prepare_(intervals);
  var result = [];
  for(var i = 0; i < intervals.length; i++) {
    var altered = this.altered_(plan, i);
    result.push(altered);
  }
  return result;
};

/**
 * @param {Array.<ble.interval.AdjustableInterval>} intervals
 * @return {Object}
 */
ble.interval.Tweaker.prototype.prepare_ = goog.abstractMethod;

/**
 * @param {Object} plan
 * @param {number} index
 * @return {ble.interval.AdjustableInterval}
 */
ble.interval.Tweaker.prototype.altered_ = goog.abstractMethod;

/**
 * @constructor
 * @extends {ble.interval.Tweaker}
 */
ble.interval.NoOverlap = function() {};
goog.inherits(ble.interval.NoOverlap, ble.interval.Tweaker);

ble.interval.NoOverlap.prototype.prepare_ = function(intervals) {
  var plan;  
  if(ble.util.isSortedBy(ble.interval.startRank, intervals)) {
    plan = intervals;
  } else {
    plan = intervals.slice(0);
    plan.sort(ble.util.comparatorFromRank(ble.interval.startRank));
  }
  return plan;
};

ble.interval.NoOverlap.prototype.altered_ = function(plan, index) {
  if(index == 0) {
    return plan[index];
  } else {
    var interval = plan[index];
    var lastInterval = plan[index - 1];
    if(interval.start() < lastInterval.end()) {
      return interval.withStartAt(lastInterval.end());
    } else {
      return interval;
    }
  }
};

/**
 * @constructor
 * @extends {ble.interval.Tweaker}
 */
ble.interval.Gapless = function() {};
goog.inherits(ble.interval.Gapless, ble.interval.Tweaker);

ble.interval.Gapless.prototype.prepare_ = ble.interval.NoOverlap.prototype.prepare_;
ble.interval.Gapless.prototype.altered_ = function(plan, index) {
  if(index == 0) {
    return plan[index];
  } else {
    var lastEnd = ble.util.maxByWithin(ble.interval.endRank, plan, 0, index);
    var interval = plan[index];
    if(interval.start() > lastEnd) {
      return interval.withStartAt(lastEnd);
    } else {
      return interval;
    }
  }
};

/**
 * @constructor
 * @param {Array.<ble.interval.Interval>} data
 */
ble.interval.Fetcher = function(data) {
  this.byEnd = data.slice();
  this.byEnd.sort(this.comparator_);
};

/**
 * @const
 */
ble.interval.Fetcher.prototype.comparator_ =
  ble.util.comparatorFromRank(ble.interval.endRank);
/**
 * @const
 */
ble.interval.Fetcher.prototype.rank_ = ble.interval.endRank;


/**
 * @param {number} point
 * @return {Array.<Array.<ble.interval.Interval>>}
 */
ble.interval.Fetcher.prototype.beforeAndDuring = function(point) {
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
ble.interval.Fetcher.prototype.add = function(interval) {
  ble.util.rankBinaryInsert(this.rank_, this.byEnd, interval);
};

ble.interval.makeRandom = function(samples, size) {
  var result = [];
  for(var i = 0; i < samples; i++) {
    var a = Math.round(Math.random() * size),
        b = Math.round(Math.random() * size),
        start = Math.min(a, b),
        end = Math.max(Math.max(a, b), start + 1);
    result.push(new ble.interval.Impl(start, end)); 
  }
  return result;
}; 
