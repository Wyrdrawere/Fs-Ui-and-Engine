namespace Engine

namespace Engine.State

open Engine.System
open Engine.UI
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics


//todo: maybe put gameEvent here, for switching IState and other stuff
type IScene =
    
    abstract update: GameTime -> unit
    
    abstract receive: Input -> unit
    
    abstract draw: SpriteBatch -> unit
  
type SceneEvent<'appEvent, 'uiKey when 'uiKey : comparison> =
    | StateEvent of 'appEvent
    | OpenUI of 'uiKey
    | CloseUI
    
type SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent, 'uiKey when 'uiKey : comparison> =
    UI<'appState, SceneEvent<'appEvent, 'uiKey>, 'uiState, 'uiEvent>

[<AbstractClass>]    
type Scene<'appState, 'appEvent, 'uiState, 'uiEvent, 'uiKey when 'uiKey : comparison>(initialState: 'appState) =
    
    let mutable eventQueue: EventQueue<SceneEvent<'appEvent, 'uiKey>> = EventQueue()
    let mutable currentState: 'appState = initialState
    //todo: might not be the best way to do things. multiple, layering uis could be made, or just some that are annoying to layout otherwise
    //todo: or upgrade layout to allow for this sort of thing (layering hard, better layouting easy)
    let mutable uiMap: Map<'uiKey, SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent, 'uiKey>> = Map.empty
    let mutable ui: SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent, 'uiKey> option = None
    
    //todo: should be event, but leads to the horrible generic type mess that was just fixed. find another way if possible
    member this.addUI(key: 'uiKey)(ui: SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent, 'uiKey>) =
        uiMap <- uiMap |> Map.add(key)(ui)
        
    member this.hasUI() =
        ui |> Option.isSome
        
    member this.pushEvent(event: SceneEvent<'appEvent, 'uiKey>) =
        eventQueue.push(event)
    
    abstract update: GameTime -> 'appState -> 'appState
    
    abstract receiveInput: Input -> 'appState -> 'appState
    
    abstract receiveEvent: 'appEvent -> 'appState -> 'appState
    
    //todo: needs some initializing function in case it doesn't get overridden. 
    abstract chooseUI:
        'uiKey ->
        Map<'uiKey, SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent, 'uiKey>> ->
        'appState ->
        SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent, 'uiKey> option
    default this.chooseUI(key: 'uiKey)(uiMap: Map<_,_>)(_: 'appState) =
        uiMap |> Map.tryFind(key)
    
    abstract render: SpriteBatch -> 'appState -> unit
    
    interface IScene with
        override this.update(gameTime: GameTime) =
            match ui with
            | Some(ui) ->
                ui.update(currentState, gameTime, eventQueue)
            | None ->
                ()
                        
            currentState <-
                currentState
                |> this.update(gameTime)
                |> (fun state ->
                    eventQueue.read() |> List.fold(fun accState sceneEvent ->
                        match sceneEvent with
                        | StateEvent(event) -> this.receiveEvent(event)(accState)
                        | OpenUI(key) ->
                            ui <- this.chooseUI(key)(uiMap)(currentState)
                            accState
                        | CloseUI ->
                            ui <- None
                            accState
                        )
                        (state))
            eventQueue <- EventQueue()
                
        override this.receive(input: Input) =
            //todo: same as before, this needs a way to decide if both ui and state get input
            //todo: maybe a function 'state -> bool, to preserve generality, with default "false"
            match ui with
            | Some(ui) ->
                ui.receive(input) 
            | None ->
                currentState <- currentState |> this.receiveInput(input)
            
        override this.draw(spriteBatch) =
            this.render(spriteBatch)(currentState)
            match ui with
            | Some(ui) -> ui.render(spriteBatch)
            | None -> ()
    
    