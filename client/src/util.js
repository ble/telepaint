goog.provide('ble.erlToMillis');
goog.provide('ble.erlToDate');

/**
 * @param {Array.<number>}
 * @return {number}
 */
ble.erlToMillis = function(when) {
  return (when[2] / 1000) + 1000 * (when[1] + 1000000 * when[0]);
};

/**
 * @param {Array.<number>}
 * @return {Date}
 */
ble.erlToDate = function(when) {
  return new Date(ble.erlToMillis(when));
};
