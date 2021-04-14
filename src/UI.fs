namespace Engine

namespace Engine.UI

open Engine.System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

[<AbstractClass>]
type UI<'appState, 'appEvent, 'uiState, 'uiEvent>(initialUIState: 'uiState, box: Box) =
     
    let mutable uiQueue: EventQueue<'uiEvent> = EventQueue()
    let mutable widgetQueue: EventQueue<Event<'appEvent, 'uiEvent>> = EventQueue()
    let mutable appQueue: EventQueue<'appEvent> = EventQueue()
    let mutable currentState = initialUIState    
    
    let mutable layouts = Map.empty
    let mutable currentLayout = 0
      
    member val Widget = Panel(WsPanel.New([Label(WsLabel.New("UI not implemented"))])) :> IWidget<'appEvent, 'uiEvent> with get, set
    
    member private this.updateLayout() =
        currentLayout <- Layout.hashParameters(this.Widget.LayoutNode)
        if not (layouts |> Map.containsKey(currentLayout))
        then
            this.Widget.initBox(Layout.calculate(box, this.Widget.initView().LayoutNode))
            let key = Layout.hashParameters(this.Widget.LayoutNode)
            let layout = Layout.calculate(box, this.Widget.LayoutNode)
            layouts <- layouts |> Map.add(key)(layout)
            this.Widget.passBox(layout)
            
    member private this.handleWidgetEvents() =
        //todo: make appQueue a ui value, that functions can push events on, to make global-ish events like "closeMenu" possible
        let mutable appQueue = EventQueue()
        for event in widgetQueue.read() do
            match event with
            | AppEvent(event) -> appQueue.push(event)
            | UIEvent(event) -> this.pushUIEvent(event)
            | _ -> ()
        appQueue
    
    member private this.handleUIEventsInternal() =
        currentState <-
            uiQueue.read()
            |> List.fold(fun state event -> this.handleUIEvent(event)(state))(currentState)
        uiQueue <- EventQueue()
    
    member this.pushUIEvent(event: 'uiEvent) =
        uiQueue.push(event)
        
    member this.pushAppEvent(event: 'appEvent) =
        appQueue.push(event)
    
    abstract synchronize: 'appState -> 'uiState -> 'uiState  
    abstract handleUIEvent: 'uiEvent -> 'uiState -> 'uiState
    
    abstract drawExtra: SpriteBatch -> 'uiState -> Unit
    default this.drawExtra(_: SpriteBatch)(_: 'uiState) = ()
    
    abstract receive: Input -> Unit
    default this.receive(input) =
        this.Widget.receive(input)(widgetQueue)
    abstract update: 'appState * GameTime -> EventQueue<'appEvent>
    default this.update(appState: 'appState, gameTime: GameTime) =
        
        currentState <- this.synchronize(appState)(currentState)
        
        this.updateLayout()
        this.Widget.update(gameTime)(widgetQueue)
            
        let mutable appQueue = this.handleWidgetEvents()
            
        widgetQueue <- EventQueue()
        this.handleUIEventsInternal()
        appQueue
        
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
        