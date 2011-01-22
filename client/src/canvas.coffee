canvasHandler = (canvas, onStroke) ->
  up = "up"
  down = "down"
  doodleColor = "#000000"

  #helpers
  ctxOf = () ->
    canvas.getContext("2d")

  getLoc = (event) ->
    origin = canvas.cumulativeOffset()
    x: event.pointerX() - origin.left
    y: event.pointerY() - origin.top

  addLoc = (coords, loc) ->
    coords.unshift loc.x, loc.y

  #handler state
  enabled = false
  up = false
  down = true
  context = null
  mouse = up
  points = []

  #external API
  module =
    drawStroke: (obj) ->
      color = obj.color if obj.color isnt undefined
      color = "#000000" if obj.color is undefined
      xys = obj.coordinates.slice()
      ctxPrivate = ctxOf()
      ctxPrivate.strokeStyle = color
      ctxPrivate.beginPath()
      ctxPrivate.moveTo xys.shift(), xys.shift()
      while xys.length > 1
        ctxPrivate.lineTo xys.shift(), xys.shift()
      ctxPrivate.stroke()
    clear: () ->
      _ctx = ctxOf(canvas)
      _ctx.clearRect(0, 0, canvas.getWidth(), canvas.getHeight())
    getImage: () ->
      canvas.toDataURL()
    setDoodleColor: (color) ->
      doodleColor = color
    enable: () ->
      enabled = true
      mouse = up
      points = []
      context = null
    disable: () ->
      enabled = false
      if mouse is down
        onStroke(
          coordinates: points.slice()
          color: doodleColor
        )
      mouse = up
      points = []
      context = null

      
  #canvas interaction handlers
  handleDown = (event) ->
    if enabled
      mouse = down
      loc = getLoc(event)
      addLoc points, loc
      context = ctxOf(canvas)
      context.strokeStyle = doodleColor
      context.beginPath()
      context.moveTo(loc.x, loc.y)

  handleEnter = (event) ->
    if enabled and event.buttons isnt 0
      handleDown event

  handleUp = (event) ->
    handleMove(event)
    if enabled and mouse is down
      mouse = up
      onStroke(
        coordinates: points.slice()
        color: doodleColor
      )
      points = []
      
  handleMove = (event) ->
    if enabled and mouse is down
      loc = getLoc(event)
      addLoc points, loc
      context.lineTo(loc.x, loc.y)
      context.stroke()

  #binding
  canvas.observe('mousedown', handleDown)
  canvas.observe('mouseup', handleUp)
  canvas.observe('mousemove', handleMove)
  canvas.observe('mouseout', handleUp)
  #return
  module


window.canvasHandler = canvasHandler
