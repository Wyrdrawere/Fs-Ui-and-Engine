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
  
type SceneEvent<'uiKey when 'uiKey : comparison> =
    | OpenUI of 'uiKey
    | CloseUI
    
type SceneUIX<'appState, 'uiState, 'appEvent, 'uiEvent, 'uiKey when 'uiKey : comparison> =
    UI<'appState, 'uiState, SceneEvent<'uiKey>, 'appEvent, 'uiEvent>

[<AbstractClass>]    
type Scene<'appState, 'uiState, 'appEvent, 'uiEvent, 'uiKey when 'uiKey : comparison>(initialState: 'appState) =
    
    let mutable eventQueue: EventQueue<GameEvent<SceneEvent<'uiKey>, 'appEvent, 'uiEvent>> = EventQueue()
    let mutable currentState: 'appState = initialState
    //todo: might not be the best way to do things. multiple, layering uis could be made, or just some that are annoying to layout otherwise
    //todo: or upgrade layout to allow for this sort of thing (layering hard, better layouting easy)
    let mutable uiMap: Map<'uiKey, SceneUIX<'appState, 'uiState, 'appEvent, 'uiEvent, 'uiKey>> = Map.empty
    let mutable ui: SceneUIX<'appState, 'uiState, 'appEvent, 'uiEvent, 'uiKey> option = None
    
    //todo: should be event, but leads to the horrible generic type mess that was just fixed. find another way if possible
    member this.addUI(key: 'uiKey)(ui: SceneUIX<'appState, 'uiState, 'appEvent, 'uiEvent, 'uiKey>) =
        uiMap <- uiMap |> Map.add(key)(ui)
        
    member this.hasUI() =
        ui |> Option.isSome
        
    member this.pushEvent(event: SceneEvent<'uiKey>) =
        eventQueue.push(SceneEvent(event))
    
    abstract update: GameTime -> 'appState -> 'appState
    
    //todo: define IState that gets input/time and queue to make scene completely automated 
    abstract receiveInput: Input -> 'appState -> 'appState
    
    abstract receiveEvent: 'appEvent -> 'appState -> 'appState
    
    //todo: needs some initializing function in case it doesn't get overridden. 
    abstract chooseUI:
        'uiKey ->
        Map<'uiKey, SceneUIX<'appState, 'uiState, 'appEvent, 'uiEvent, 'uiKey>> ->
        'appState ->
        SceneUIX<'appState, 'uiState, 'appEvent, 'uiEvent, 'uiKey> option
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
                    eventQueue.read() |> List.fold(fun accState generalEvent ->
                        match generalEvent with
                        | AppEvent(event) ->
                            accState |> this.receiveEvent(event)
                        | _ -> accState
                        )
                        (state))
                
            for event in eventQueue.read() do
                match event with
                | SceneEvent(event) ->
                    match event with
                    | OpenUI(key) ->
                        ui <- this.chooseUI(key)(uiMap)(currentState)
                    | CloseUI ->
                        ui <- None
                | _ -> ()
                        
                
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
    
    