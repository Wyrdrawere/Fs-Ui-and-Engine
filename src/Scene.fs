namespace Engine

namespace Engine.State

open Engine.System
open Engine.UI
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics



type SceneEvent<'nexusEvent, 'uiKey when 'uiKey : comparison> =
    | OpenUI of 'uiKey
    | CloseUI
    | NexusEvent of 'nexusEvent
    
//todo: maybe put gameEvent here, for switching IState and other stuff
type IScene<'nexusEvent> =
    
    abstract receive: Input -> unit
    
    abstract update: EventQueue<'nexusEvent> -> GameTime -> unit
    
    abstract draw: SpriteBatch -> unit
  
[<AbstractClass>]    
type Scene<'appState, 'uiState, 'appEvent, 'uiEvent, 'nexusEvent, 'uiKey when 'uiKey : comparison>(initialState: 'appState) =
    
    let mutable eventQueue: EventQueue<GameEvent<SceneEvent<'nexusEvent, 'uiKey>, 'appEvent, 'uiEvent>> = EventQueue()
    let mutable currentState: 'appState = initialState
    //todo: might not be the best way to do things. multiple, layering uis could be made, or just some that are annoying to layout otherwise
    //todo: or upgrade layout to allow for this sort of thing (layering hard, better layouting easy)
    //todo: uis can now replace themselves with other uis, layering them could be done by replacing option with list. 
    let mutable uiMap: Map<'uiKey, SceneUI<'appState, 'uiState, SceneEvent<'nexusEvent, 'uiKey>, 'appEvent, 'uiEvent>> = Map.empty
    let mutable ui: SceneUI<'appState, 'uiState, SceneEvent<'nexusEvent, 'uiKey>, 'appEvent, 'uiEvent> option = None
    
    //todo: should be event, but leads to the horrible generic type mess that was just fixed. find another way if possible
    member this.addUI(key: 'uiKey)(ui: SceneUI<'appState, 'uiState, SceneEvent<'nexusEvent, 'uiKey>, 'appEvent, 'uiEvent>) =
        uiMap <- uiMap |> Map.add(key)(ui)
        
    member this.hasUI() =
        ui |> Option.isSome
        
    //todo: think about changing this to pushSceneEvent and adding pushAppEvent, then gameState wont have to implement own queue    
    member this.pushEvent(event: SceneEvent<'nexusEvent, 'uiKey>) =
        eventQueue.push(SceneEvent(event))
    
    abstract update: GameTime -> 'appState -> 'appState
    
    abstract handleInput: Input -> 'appState -> 'appState
    
    abstract handleEvent: 'appEvent -> 'appState -> 'appState
    
    //todo: needs some initializing function in case it doesn't get overridden. 
    abstract chooseUI:
        'uiKey ->
        Map<'uiKey, SceneUI<'appState, 'uiState, SceneEvent<'nexusEvent, 'uiKey>, 'appEvent, 'uiEvent>> ->
        'appState ->
        SceneUI<'appState, 'uiState, SceneEvent<'nexusEvent, 'uiKey>, 'appEvent, 'uiEvent> option
    default this.chooseUI(key: 'uiKey)(uiMap: Map<_,_>)(_: 'appState) =
        uiMap |> Map.tryFind(key)
    
    abstract render: SpriteBatch -> 'appState -> unit
    
    interface IScene<'nexusEvent> with
        override this.update(nexusQueue: EventQueue<'nexusEvent>)(gameTime: GameTime) =
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
                            accState |> this.handleEvent(event)
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
                    | NexusEvent(event) ->
                        nexusQueue.push(event)
                | _ -> ()
                        
                
            eventQueue <- EventQueue()
                
        override this.receive(input: Input) =
            //todo: same as before, this needs a way to decide if both ui and state get input
            //todo: maybe a function 'state -> bool, to preserve generality, with default "false"
            match ui with
            | Some(ui) ->
                ui.handleInput(input) 
            | None ->
                currentState <- currentState |> this.handleInput(input)
            
        override this.draw(spriteBatch) =
            this.render(spriteBatch)(currentState)
            match ui with
            | Some(ui) -> ui.render(spriteBatch)
            | None -> ()
    
    