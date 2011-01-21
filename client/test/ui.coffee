#abstract behaviors
toggler = (turnOn, turnOff) ->
  state = "off"
  () ->
    if state == "on"
      turnOff()
      state = "off"
    else if state == "off"
      turnOn()
      state = "on"

#display object for chat events
chatManager = (playerContainer, chatContainer) ->
  players = []
  colorMax = 11
  colorStep = 3
  delta = 2 * Math.PI * colorStep / colorMax
  showingStacks = false
  updateDisplay = () ->
    x.element.update("#{x.name}: #{x.count}") for x in players if showingStacks
    x.element.update("#{x.name}") for x in players if not showingStacks
  module =
    addPlayer: (name) ->
      color = "#" + colorWheel(delta * players.length)
      element = new Element("li", {style: "color: #{color};"})
      playerContainer.insert(element)
      players.push(
        element: element
        name: name
        color: color
        count: 0
      )
      updateDisplay()
    countPlayers: () ->
      players.length
    toggleStacks: () ->
      showingStacks = not showingStacks
      updateDisplay()
    chat: (n, text) ->
      player = players[n]
      element = new Element("li", {style: "color: #{player.color};"})
      element.update(text)
      chatContainer.insert(element)
      chatContainer.scrollTop = chatContainer.scrollHeight

#display object for managing a pile (i.e. multiple stacks)
stacksManager = (container) ->
  stacks = []
  module =
    addToStacks: (topImage) ->
      inner = new Element("div", {class: "miniContainer"})
      inner.insert(new Element("div", {class: "mini mini4"}))
      inner.insert(new Element("div", {class: "mini mini3"}))
      inner.insert(new Element("div", {class: "mini mini2"}))
      if stacks.length == 0
        classes = "stackTop mini mini1"
      else
        classes = "mini mini1"
      top = new Element("img", {class: classes, src: topImage})
      inner.insert(top)
      container.insert(inner)
      stacks.push([inner, top])
    clearStacks: () ->
      stack[0].remove() for stack in stacks
      stacks = []
    removeStack: () ->
      [inner, ignored] = stacks.shift()
      inner.remove()
      if stacks.length > 0
        stacks[0][1].addClassName("stackTop")

#display object for managing a review stack
stackManager = (container) ->
  images = (new Element("img", {width: 480, height: 360}) for index in [0,1])
  container.insert(image) for image in images
  updateDisplay = (urls) ->
    for index in [0..images.length-1]
      url = urls[index] if index < urls.length
      url = "" if index >= urls.length
      images[index].writeAttribute("src", url)
  model = rotater([], updateDisplay)
  module =
    setStack: (stack) ->
      urls = (sheet.url for sheet in stack.sheets)
      model.replace(urls)
    rotateStack: () ->
      model.rotate()

#HOF/object for updating an object with its state stored in an array
rotater = (start, onUpdate) ->
  state = start.slice()
  module =
    rotate: () ->
      state.push(state.shift())
      onUpdate(state)
    add: (item) ->
      state.push(item)
      onUpdate(state)
    replace: (newState) ->
      state = newState.slice()
      onUpdate(state)

reviewPileManager = (container, stackContainer) ->
  stacks = stacksManager(container)
  reviewStack = stackManager(stackContainer)
  updateDisplay = (state) ->
    topStack = state[0] if state.length > 0
    topStack = [] if state.length == 0
    reviewStack.setStack(topStack)
    stacks.clearStacks()
    stacks.addToStacks(stack.topImage) for stack in state
  module = rotater([], updateDisplay)
  module




bindLayer = (layer) ->
  layer = Element.extend(layer)
  result =
    show: () -> layer.setStyle({visibility: "visible"})
    hide: () -> layer.setStyle({visibility: "hidden"})
window.toggler = toggler
window.chatManager = chatManager
window.stackManager = stackManager
window.rotater = rotater
window.reviewPileManager = reviewPileManager
window.bindLayer = bindLayer
window.stacksManager = stacksManager
