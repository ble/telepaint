(function() {
  var PI, colorWheel, hexFormatColor, hexPad;
  PI = Math.PI;
  hexPad = function(number, digits) {
    var result;
    result = number.toString(16);
    while (result.length < digits) {
      result = "0" + result;
    }
    return result;
  };
  hexFormatColor = function(red, green, blue) {
    red = Math.floor(red);
    green = Math.floor(green);
    blue = Math.floor(blue);
    return hexPad(red, 2) + hexPad(green, 2) + hexPad(blue, 2);
  };
  colorWheel = function(radians, lum) {
    var amtMinor, blue, center, green, ixMajor, ixMinor, red;
    if (lum === void 0) {
      lum = 1.0;
    }
    while (radians > 2 * PI) {
      radians -= 2 * PI;
    }
    while (radians < 0) {
      radians += 2 * PI;
    }
    ixMajor = Math.floor(radians / (2 * PI / 3)) % 3;
    ixMinor = (8 - Math.floor(radians / (PI / 3))) % 3;
    center = (1 + 2 * ixMajor) * PI / 3;
    amtMinor = 1.5 * Math.abs(radians - center);
    red = 0;
    if (ixMajor === 0) {
      red = 255;
    }
    if (ixMinor === 0) {
      red = 255 * Math.sin(amtMinor);
    }
    green = 0;
    if (ixMajor === 1) {
      green = 255;
    }
    if (ixMinor === 1) {
      green = 255 * Math.sin(amtMinor);
    }
    blue = 0;
    if (ixMajor === 2) {
      blue = 255;
    }
    if (ixMinor === 2) {
      blue = 255 * Math.sin(amtMinor);
    }
    return hexFormatColor(lum * red, lum * green, lum * blue);
  };
  window.colorWheel = colorWheel;
}).call(this);
