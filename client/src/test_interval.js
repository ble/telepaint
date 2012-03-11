

var I = function(start, end) { return new ble.interval.Impl(start, end); };

var case0 = [I(-10, 10), I(-5, 10), I(0, 10), I(5, 10), I(9, 12), I(15, 20)];
var case1 = [I(0, 1), I(2, 3), I(4, 5), I(6, 7), I(6.5, 9), I(7, 8), I(10, 11)];

var show = function(xs) { xs.forEach(function(x) { console.log([x.start(), x.end()]); })};

var test = function(input) {
  console.log("before");
  show(input);
  console.log("after");
  show(ble.interval.withoutGaps(input));
};

