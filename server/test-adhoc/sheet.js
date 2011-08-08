

var whenState = function(xhr, state, fn) {
  return function() {
    if(xhr.readyState == state) {
      return fn(xhr.response);
    }
  }
};

var bind = function(fn, selfObj, var_args) {
  if(!fn)
    throw new Error();
  if(arguments.length > 2) {
    var boundArgs = Array.prototype.slice.call(arguments, 2);
    return function() {
      var newArgs = Array.prototype.slice.call(arguments);
      Array.prototype.unshift.apply(newArgs, boundArgs);
      return fn.apply(selfObj, newArgs);
    };
  } else {
    return function() {
      return fn.apply(selfObj, arguments);
    };
  }
};

var sheetAppend = function(method, data) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', document.location.toString());
  var rpc = ({'method': method, 'data': data});
  var onDone = bind(console.log, console);
  xhr.onreadystatechange = whenState(xhr, xhr.DONE, onDone);
  xhr.send(JSON.stringify(rpc));
};

var sheetRead = function() {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', document.location.toString());
  var onDone = function(x) { console.log(JSON.parse(x)); };
  xhr.onreadystatechange = whenState(xhr, xhr.DONE, onDone);
  xhr.send();
};

