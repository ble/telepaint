goog.provide('ble.gfx');

ble.gfx.pathPixCoords = function(ctx, coords) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(0.5 + coords[0], 0.5 + coords[1]);
  for(var i = 1; i < coords.length / 2; i++)
    ctx.lineTo(0.5 + coords[2*i], 0.5 + coords[2*i+1]);
}

ble.gfx.pathPixCoordsWithin = function(ctx, coords, start, last) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(0.5 + coords[2*start], 0.5 + coords[2*start+1]);
  for(var i = start+1; i <= last; i++) {
    ctx.lineTo(0.5 + coords[2*i], 0.5 + coords[2*i+1]);
  }
}

ble.gfx.pathCoords = function(ctx, coords) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[0], coords[1]);
  for(var i = 1; i < coords.length / 2; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
}

ble.gfx.pathCoordsWithin = function(ctx, coords, start, last) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[2*start], coords[2*start+1]);
  for(var i = start+1; i <= last; i++) {
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  }
}

