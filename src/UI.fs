namespace Engine

namespace Engine.UI

open Engine.System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

[<AbstractClass>]
type UI<'appState, 'uiState, 'sceneEvent, 'appEvent, 'uiEvent>(initialUIState: 'uiState, box: Box) =
     
    let mutable eventQueue: EventQueue<GameEvent<'sceneEvent, 'appEvent, 'uiEvent>> = EventQueue()
    let mutable currentState = initialUIState    
    
    let mutable layouts = Map.empty
    let mutable currentLayout = 0
      
    member val Widget: IWidget<GameEvent<'sceneEvent, 'appEvent, 'uiEvent>> =
        Panel(WsPanel.New([]))
        :> IWidget<GameEvent<'sceneEvent, 'appEvent, 'uiEvent>> with get, set
    
    member this.getState() =
        currentState
    
    //looks absolutely useless, but is really handy in inherited classes
    member this.asUI() =
        this :> UI<'appState, 'uiState, 'sceneEvent, 'appEvent, 'uiEvent>
    
    member private this.updateLayout() =
        currentLayout <- Layout.hashParameters(this.Widget.LayoutNode)
        if not (layouts |> Map.containsKey(currentLayout))
        then
            this.Widget.initBox(Layout.calculate(box, this.Widget.initView().LayoutNode))
            let key = Layout.hashParameters(this.Widget.LayoutNode)
            let layout = Layout.calculate(box, this.Widget.LayoutNode)
            layouts <- layouts |> Map.add(key)(layout)
            this.Widget.passBox(layout)
            
    member this.pushEvent(event: GameEvent<'sceneEvent, 'appEvent, 'uiEvent>) =
        eventQueue.push(event)
            
    abstract synchronize: 'appState -> 'uiState -> 'uiState  
    abstract handleUIEvent: 'uiEvent -> 'uiState -> 'uiState
    
    abstract drawExtra: SpriteBatch -> 'uiState -> Unit
    default this.drawExtra(_: SpriteBatch)(_: 'uiState) = ()
    
    abstract receive: Input -> unit
    default this.receive(input: Input) =
        this.Widget.receive(input)(eventQueue)
        
    abstract update: 'appState * GameTime * EventQueue<GameEvent<'sceneEvent, 'appEvent, 'uiEvent>> -> unit
    default this.update(appState: 'appState, gameTime: GameTime, appQueue: EventQueue<GameEvent<'sceneEvent, 'appEvent, 'uiEvent>>) =
        
        currentState <- this.synchronize(appState)(currentState)
        
        this.updateLayout()
        this.Widget.update(gameTime)(eventQueue)
        
        for event in eventQueue.read() do
            match event with
            | SceneEvent _ as event -> appQueue.push(event)
            | AppEvent _ as event -> appQueue.push(event)
            | UIEvent(event) -> currentState <- this.handleUIEvent(event)(currentState)
            
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
        