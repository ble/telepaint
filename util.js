goog.provide('ble.util.binarySearch');

ble.util.binarySearch = function(array, value) {
  if(array.length == 0)
    return -1;
  return ble.util.binarySearch0(array, 0, array.length-1, Math.floor(array.length / 2), value);
};

ble.util.binarySearch0 = function(array, ix0, ixF, ix, value) {
  var ixVal = array[ix];
  if(ixVal == value) {
    return ix;
  }
  
  else if(ix0 == ix || ixF == ix) {
    if(ixVal > value)
      return ix - 0.5;
    if(ixVal < value)
      return ix + 0.5;
  }

  else if(ixVal > value) {
    return ble.util.binarySearch0(array, ix0, ix, Math.floor((ix0 + ix) / 2), value);
  }  else { 
    return ble.util.binarySearch0(array, ix, ixF, Math.ceil((ix + ixF) / 2), value);
  }
}


