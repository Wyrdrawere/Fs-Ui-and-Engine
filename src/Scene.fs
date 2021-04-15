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
  
type SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent> =
    UI<'appState, SceneEvent<'appState, 'appEvent, 'uiState, 'uiEvent>, 'uiState, 'uiEvent>
and
    SceneEvent<'appState, 'appEvent, 'uiState, 'uiEvent> =
    | StateEvent of 'appEvent
    | OpenUI of SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent>
    | CloseUI 

[<AbstractClass>]    
type Scene<'appState, 'appEvent, 'uiState, 'uiEvent>(initialState: 'appState) =
    
    let mutable eventQueue: EventQueue<SceneEvent<'appState, 'appEvent, 'uiState, 'uiEvent>> = EventQueue()
    let mutable currentState: 'appState = initialState
    //todo: might not be the best way to do things. multiple, layering uis could be made, or just some that are annoying to layout otherwise
    //todo: or upgrade layout to allow for this sort of thing (layering hard, better layouting easy)
    let mutable ui: SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent> option = None
    
    member this.setUI(nextUI: SceneUI<'appState, 'appEvent, 'uiState, 'uiEvent> option) =
        ui <- nextUI
        
    member this.hasUI() =
        ui |> Option.isSome
    
    abstract update: GameTime -> 'appState -> 'appState
    
    abstract receiveInput: Input -> 'appState -> 'appState
    
    abstract receiveEvent: 'appEvent -> 'appState -> 'appState
    
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
                        | OpenUI(nextUI) ->
                            ui <- Some(nextUI)
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
    
    