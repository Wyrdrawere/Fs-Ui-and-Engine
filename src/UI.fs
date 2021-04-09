namespace Engine

namespace Engine.UI

open Engine.System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

[<AbstractClass>]
type UI<'appState, 'appEvent, 'uiState, 'uiEvent>(initialUIState: 'uiState, box: Box) =
     
    let mutable uiQueue: EventQueue<'uiEvent> = EventQueue()
    let mutable widgetQueue: EventQueue<Event<'appEvent, 'uiEvent>> = EventQueue()
    let mutable currentState = initialUIState    
    
    let mutable layouts = Map.empty
    let mutable currentLayout = 0
      
    //todo: find way to make delay another let and make widget write-only from outside/inherited classes
    member val Delay = Engine.Config.inputDelay with get, set
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
            
    member private this.handleDelayAndUpdate(gameTime: GameTime, input: EventQueue<KeyInput>) =
        let delta = (float32 gameTime.ElapsedGameTime.Milliseconds) / 1000.0f
        if this.Delay > delta
        then
            this.Delay <- this.Delay - delta
        else
            this.Delay <- 0.0f
            this.Widget.receive(input)(widgetQueue)
    
    member private this.handleWidgetEvents() =
        let mutable delayed = false
        let mutable appQueue = EventQueue()
        for event in widgetQueue.read() do
            match event with
            | SetDelay when not delayed ->
                this.Delay <- Engine.Config.inputDelay
                delayed <- true
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
    
    abstract synchronize: 'appState -> 'uiState -> 'uiState  
    abstract handleUIEvent: 'uiEvent -> 'uiState -> 'uiState
    
    //todo: this exists to add uiEvents based on appEvents. decide if buttons should be allowed to push multiple events, then this could go
    abstract inspectAppEvents: EventQueue<'appEvent> -> EventQueue<'appEvent>
    default this.inspectAppEvents(queue: EventQueue<'appEvent>) = queue
    abstract drawExtra: SpriteBatch -> 'uiState -> Unit
    default this.drawExtra(_: SpriteBatch)(_: 'uiState) = ()
    
    abstract update: 'appState * GameTime * EventQueue<KeyInput> -> EventQueue<'appEvent>
    default this.update(appState: 'appState, gameTime: GameTime, input: EventQueue<KeyInput>) =
        
        currentState <- this.synchronize(appState)(currentState)
        
        this.updateLayout()
        this.Widget.update(gameTime)(widgetQueue)
        
        this.handleDelayAndUpdate(gameTime, input)
            
        let mutable appQueue = this.handleWidgetEvents()
            
        widgetQueue <- EventQueue()
        appQueue <- this.inspectAppEvents(appQueue)
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
        