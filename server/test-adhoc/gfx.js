goog.provide('ble.gfx');

ble.gfx.strokeCoords = function(ctx, coords) {
  if(coords.length == 0)
    return;
  ctx.beginPath();
  ctx.moveTo(coords[0], coords[1]);
  for(var i = 1; i < coords.length / 2; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  ctx.stroke();
}

ble.gfx.strokeCoordsWithin = function(ctx, coords, start, last) {
  if(coords.length == 0)
    return;
  ctx.beginPath();
  ctx.moveTo(coords[2*start], coords[2*start+1]);
  for(var i = start+1; i <= last; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  ctx.stroke();
}

