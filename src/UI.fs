namespace Engine

namespace Engine.UI

open Engine.System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

//todo: maybe make inEvent/outEvent or something, to allow for defintion of translate/extract function and make sceneUI less dumb
[<AbstractClass>]
type UI<'appState, 'uiState, 'event>(initialUIState: 'uiState, box: Box) =
     
    let mutable eventQueue: EventQueue<'event> = EventQueue()
    let mutable currentState = initialUIState
    
    let mutable layouts = Map.empty
    let mutable currentLayout = 0
      
    member val Widget: IWidget<'event> =
        Panel(WsPanel.New([]))
        :> IWidget<'event> with get, set
    
    member this.getState() =
        currentState
    
    
    member private this.updateLayout() =
        currentLayout <- Layout.hashParameters(this.Widget.LayoutNode)
        if not (layouts |> Map.containsKey(currentLayout))
        then
            this.Widget.initBox(Layout.calculate(box, this.Widget.initView().LayoutNode))
            let key = Layout.hashParameters(this.Widget.LayoutNode)
            let layout = Layout.calculate(box, this.Widget.LayoutNode)
            layouts <- layouts |> Map.add(key)(layout)
            this.Widget.passBox(layout)
            
    member this.pushEvent(event: 'event) =
        eventQueue.push(event)
            
    abstract synchronize: 'appState -> 'uiState -> 'uiState  
    abstract handleEvent: 'event -> 'uiState -> 'uiState
    abstract receiveAndEmit: EventQueue<'event> -> EventQueue<'event> -> 'uiState -> 'uiState
    
    abstract drawExtra: SpriteBatch -> 'uiState -> Unit
    default this.drawExtra(_: SpriteBatch)(_: 'uiState) = ()
    
    abstract handleInput: Input -> unit
    default this.handleInput(input: Input) =
        this.Widget.receive(input)(eventQueue)
        
    abstract update: 'appState * GameTime * EventQueue<'event> -> unit
    default this.update(appState: 'appState, gameTime: GameTime, appQueue: EventQueue<'event>) =
        
        currentState <- this.synchronize(appState)(currentState)
        
        this.updateLayout()
        this.Widget.update(gameTime)(eventQueue)
        
        currentState <- this.receiveAndEmit(eventQueue)(appQueue)(currentState)
        
        eventQueue <- EventQueue()
        
        
    abstract render: SpriteBatch -> Unit
    default this.render(spriteBatch: SpriteBatch) =
        this.updateLayout()
        match layouts |> Map.tryFind(currentLayout) with
        | Some(layout) ->
            this.Widget.draw(spriteBatch)(layout)
            this.drawExtra(spriteBatch)(currentState)
        | None ->
            this.updateLayout()
            this.render(spriteBatch)
        
[<AbstractClass>]
type SceneUI<'appState, 'uiState, 'sceneEvent, 'appEvent, 'uiEvent>(initialUIState: 'uiState, box: Box) =
    
    inherit UI<'appState, 'uiState, GameEvent<'sceneEvent, 'appEvent, 'uiEvent>>(initialUIState, box) with
        
        override this.receiveAndEmit(inQueue: EventQueue<_>)(outQueue: EventQueue<_>)(state: 'uiState) =
            
            let mutable updatedState = state
        
            for event in inQueue.read() do
                match event with
                | SceneEvent _ as event -> outQueue.push(event)
                | AppEvent _ as event -> outQueue.push(event)
                | UIEvent _ as event -> updatedState <- this.handleEvent(event)(updatedState)
                
            updatedState
                
        
        
   