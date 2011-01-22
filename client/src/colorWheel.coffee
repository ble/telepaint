PI = Math.PI

hexPad = (number, digits) ->
  result = number.toString(16)
  result = "0" + result while (result.length < digits)
  result

hexFormatColor = (red, green, blue) ->
  red = Math.floor(red)
  green = Math.floor(green)
  blue = Math.floor(blue)
  hexPad(red, 2) + hexPad(green, 2) + hexPad(blue, 2)

colorWheel = (radians, lum) ->
  lum = 1.0 if lum == undefined
  radians -= 2*PI while radians > 2*PI
  radians += 2*PI while radians < 0
  ixMajor = Math.floor(radians / (2*PI/3)) % 3
  ixMinor = (8 - Math.floor(radians / (PI/3))) % 3
  center = (1 + 2 * ixMajor) * PI/3
  amtMinor = 1.5 * Math.abs(radians - center)
  red = 0
  red = 255 if ixMajor == 0
  red = 255 * Math.sin(amtMinor) if ixMinor == 0
  green = 0
  green = 255 if ixMajor == 1
  green = 255 * Math.sin(amtMinor) if ixMinor == 1
  blue = 0
  blue = 255 if ixMajor == 2
  blue = 255 * Math.sin(amtMinor) if ixMinor == 2
  hexFormatColor(lum*red, lum*green, lum*blue)

window.colorWheel = colorWheel
