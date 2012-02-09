goog.provide('ble.hate');


ble.hate.links = function() {
  var links = document.head.getElementsByTagName('link');
  return ble.hate.linksToObject(links);
};

ble.hate.linksToObject = function(links) {
  var result = {};
  for(var i = 0; i < links.length; i++) {
    var link = links[i];
    var href = link.getAttribute('href');
    var rels = link.getAttribute('rel');
    if(!rels)
      continue;
    rels = rels.split(' ');
    for(var j = 0; j < rels.length; j++) {
      var rel = rels[j];
      if(!(rel in result))
        result[rel] = [];
      result[rel].push(href);
    }
  }
  return result;
};

ble.hate.addLinks = function(linkObj) {
  for(var key in linkObj) {
    if(!linkObj.hasOwnProperty(key))
      continue;
    var href = linkObj[key];
    var link = document.createElement('link');
    link.setAttribute('href', href);
    link.setAttribute('rel', key);
    document.head.appendChild(link);
  }
};
