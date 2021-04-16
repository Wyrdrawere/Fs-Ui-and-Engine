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
      
    member val Widget: IWidget<'globalEvent> option = None with get, set
       
    member this.getState() =
        currentState
    
    
    member private this.updateLayout() =
        match this.Widget with
        | Some(widget) ->
            currentLayout <- Layout.hashParameters(widget.LayoutNode)
            if not (layouts |> Map.containsKey(currentLayout))
            then
                widget.initBox(Layout.calculate(box, widget.initView().LayoutNode))
                let key = Layout.hashParameters(widget.LayoutNode)
                let layout = Layout.calculate(box, widget.LayoutNode)
                layouts <- layouts |> Map.add(key)(layout)
                widget.passBox(layout)
        | None -> ()
            
    member this.pushEvent(event: 'globalEvent) =
        eventQueue.push(event)
            
    abstract synchronize: 'appState -> 'uiState -> 'uiState  
    abstract handleEvent: 'localEvent -> 'uiState -> 'uiState
    abstract localize: 'globalEvent -> 'localEvent option
    
    abstract drawExtra: SpriteBatch -> 'uiState -> Unit
    default this.drawExtra(_: SpriteBatch)(_: 'uiState) = ()
    
    abstract handleInput: Input -> unit
    default this.handleInput(input: Input) =
        this.Widget
        |> Option.map(fun widget -> widget.receive(input)(eventQueue))
        |> ignore
        
    abstract update: 'appState * GameTime * EventQueue<'globalEvent> -> unit
    default this.update(appState: 'appState, gameTime: GameTime, appQueue: EventQueue<'globalEvent>) =
        
        currentState <- this.synchronize(appState)(currentState)
        
        this.updateLayout()
        
        this.Widget
        |> Option.map(fun widget -> widget.update(gameTime)(eventQueue))
        |> ignore
        
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
            
            this.Widget
            |> Option.map(fun widget -> widget.draw(spriteBatch)(layout))
            |> ignore
            
            this.drawExtra(spriteBatch)(currentState)
        | None when this.Widget |> Option.isSome ->
            this.updateLayout()
            this.render(spriteBatch)
        | None -> ()
        
[<AbstractClass>]
type SceneUI<'appState, 'uiState, 'sceneEvent, 'appEvent, 'uiEvent>(initialUIState: 'uiState, box: Box) =
    
    inherit UI<'appState, 'uiState, GameEvent<'sceneEvent, 'appEvent, 'uiEvent>, 'uiEvent>(initialUIState, box) with
        
        override this.localize(gameEvent: GameEvent<_,_,_>) =
            match gameEvent with
            | UIEvent(event) -> Some(event)
            | _ -> None
                
        
        
   