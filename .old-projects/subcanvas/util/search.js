goog.provide('ble.util.binarySearch');
goog.provide('ble.util.binarySearchIn');
goog.provide('ble.util.binaryInsert');
goog.provide('ble.util.binaryInsertBy');
goog.provide('ble.util.comparatorFromRank');
goog.provide('ble.util.rankBinarySearch');
goog.provide('ble.util.rankBinarySearchIn');
goog.provide('ble.util.isSortedBy');
goog.provide('ble.util.maxBy');
goog.provide('ble.util.minBy');
goog.provide('ble.util.maxByWithin');
goog.provide('ble.util.minByWithin');

/**
 * Return the index into the array of either
 *     a copy of the specified value or
 *     an insertion point between indices of values below and above or
 *     the start-of-array insertion point if the array is empty.
 * @param {Array.<number>} data values in ascending order
 * @param {number} key value to be found
 * @return {number} integer index or insertion point of the form N + 0.5
 */
ble.util.binarySearch = function(data, key) {
  return ble.util.binarySearchIn(data, key, 0, data.length - 1);
};

/**
 * @param {Array.<number>} data
 * @param {number} key
 * @param {number} startIx first index where the value might be found
 * @param {number} endIx last index where the value might be found
 * @return {number} integer index or insertion point of the form N + 0.5
 */
ble.util.binarySearchIn = function(data, key, startIx, endIx) {
  //return immediately on length 0 arrays and when the search has narrowed to
  //a single element.
  if(data.length == 0)
    return -0.5;

  if(startIx == endIx) { 
    var startVal = data[startIx];
    if(startVal == key)
      return startIx;
    else if(startVal < key)
      return startIx + 0.5;
    else if(startVal > key)
      return startIx - 0.5;
    else 
      throw "NaN found during ble.binarySearchIn";
  }

  var middleIx = Math.floor((startIx + endIx) / 2);
  var middleVal = data[middleIx];
  if(middleVal == key)
    return middleIx;
  else if(middleVal < key)
    return ble.util.binarySearchIn(data, key, middleIx + 1, endIx);
  else if(middleVal > key)
    return ble.util.binarySearchIn(data, key, startIx, middleIx);
  else 
    throw "NaN found during ble.binarySearchIn";
};

ble.util.comparatorFromRank = function(rankFn) {
  return function(a, b) {
    return rankFn(a) - rankFn(b);
  };
}

/**
 * @param {Array.<number>} data
 * @param {number} key
 */
ble.util.binaryInsert = function(data, key) {
  var insertionPoint = Math.ceil(ble.util.binarySearch(data, key));
  data.splice(insertionPoint, 0, key);
};

/**
 * Like ble.util.binarySearch, only it takes an array of arbitrary objects 
 * and a ranking function and searches for an element of the array with rank
 * equal to key.  The array of objects must already be sorted according to
 * the ranking function, which is guaranteed exactly by
 *    data.sort(ble.util.comparatorFromRank(rankFn);
 *
 * ble.util.binarySearch(d, k) should always yield the exact same answer as
 * ble.util.rankBinarySearch(function(x){return x}, d, k); essentially the
 * same function is written twice because the numeric case is important and
 * possibly faster without calls to the identity function.
 *
 * @param {function(Object): number} rankFn the ranking function
 * @param {Array.<Object>} data objects in increasing order
 * @param {number} key rank to be found
 * @return {number} integer index or insertion point of the form N + 0.5
 */
ble.util.rankBinarySearch = function(rankFn, data, key) {
  return ble.util.rankBinarySearchIn(rankFn, data, key, 0, data.length - 1);
};

ble.util.rankBinarySearchIn = function(rankFn, data, key, startIx, endIx) {
  //return immediately on length 0 arrays and when the search has narrowed to
  //a single element.
  if(data.length == 0)
    return -0.5;

  if(startIx == endIx) { 
    var startRank = rankFn(data[startIx]);
    if(startRank == key)
      return startIx;
    else if(startRank < key)
      return startIx + 0.5;
    else if(startRank > key)
      return startIx - 0.5;
    else 
      throw "NaN found during ble.binarySearchIn";
  }

  var middleIx = Math.floor((startIx + endIx) / 2);
  var middleRank = rankFn(data[middleIx]);
  if(middleRank == key)
    return middleIx;
  else if(middleRank < key)
    return ble.util.rankBinarySearchIn(rankFn, data, key, middleIx + 1, endIx);
  else if(middleRank > key)
    return ble.util.rankBinarySearchIn(rankFn, data, key, startIx, middleIx);
  else 
    throw "NaN found during ble.binarySearchIn";
};


ble.util.rankBinaryInsert = function(rankFn, data, obj) {
  var insertRank = rankFn(obj);
  var insertionPoint = Math.ceil(ble.util.rankBinarySearch(rankFn, data, insertRank));
  data.splice(insertionPoint, 0, obj);
};

ble.util.maxBy = function(rankFn, data) {
  ble.util.maxByWithin(rankFn, data, 0, data.length);
};

ble.util.maxByWithin = function(rankFn, data, start, length) {
  var ix = -1, max = -Infinity;
  for(var i = start; i < start+length; i++) {
    var sample = rankFn(data[i]);
    if(sample > max) {
      ix = i;
      max = sample;
    } 
  }
  return ix; 
};

ble.util.minBy = function(rankFn, data) {
  return ble.util.minByWithin(rankFn, data, 0, data.length);
};

ble.util.minByWithin = function(rankFn, data, start, length) {
  var ix = -1, min = Infinity;
  for(var i = start; i < start+length; i++) {
    var sample = rankFn(data[i]);
    if(sample > min) {
      ix = i;
      min = sample;
    } 
  }
  return ix; 
};

ble.util.isSortedBy = function(rankFn, data) {
  var lastRank = -Infinity;
  for(var i = 0; i < data.length; i++) {
    var rank = rankFn(data[i]);
    if(rank < lastRank)
      return false;
    lastRank = rank;
  }
  return true;
};
