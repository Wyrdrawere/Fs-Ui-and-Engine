namespace Engine

namespace Engine.UI

open Engine.System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

[<AbstractClass>]
type UI<'appState, 'uiState, 'globalEvent, 'localEvent>(initialUIState: 'uiState, box: Box) =
    
    let mutable eventQueue: EventQueue<'globalEvent> = EventQueue()
    let mutable currentState = initialUIState
    
    let mutable layouts = Map.empty
    let mutable currentLayout = 0
      
    member val Widget: IWidget<'globalEvent> =
        Panel(WsPanel.New([]))
        :> IWidget<'globalEvent> with get, set
    
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
            
    member this.pushEvent(event: 'globalEvent) =
        eventQueue.push(event)
            
    abstract synchronize: 'appState -> 'uiState -> 'uiState  
    abstract handleEvent: 'localEvent -> 'uiState -> 'uiState
    abstract localize: 'globalEvent -> 'localEvent option
    
    abstract drawExtra: SpriteBatch -> 'uiState -> Unit
    default this.drawExtra(_: SpriteBatch)(_: 'uiState) = ()
    
    abstract handleInput: Input -> unit
    default this.handleInput(input: Input) =
        this.Widget.receive(input)(eventQueue)
        
    abstract update: 'appState * GameTime * EventQueue<'globalEvent> -> unit
    default this.update(appState: 'appState, gameTime: GameTime, appQueue: EventQueue<'globalEvent>) =
        
        currentState <- this.synchronize(appState)(currentState)
        
        this.updateLayout()
        this.Widget.update(gameTime)(eventQueue)
        
        for globalEvent in eventQueue.read() do
            match this.localize(globalEvent) with
            | Some(event) ->
                currentState <- this.handleEvent(event)(currentState)
            | None ->
                appQueue.push(globalEvent)
                
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
    
    inherit UI<'appState, 'uiState, InternalEvent<'sceneEvent, 'appEvent, 'uiEvent>, 'uiEvent>(initialUIState, box) with
        
        override this.localize(gameEvent: InternalEvent<_,_,_>) =
            match gameEvent with
            | UIEvent(event) -> Some(event)
            | _ -> None
                
        
        
   