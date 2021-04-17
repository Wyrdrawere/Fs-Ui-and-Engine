namespace Engine

namespace Engine.State

open Engine.System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type SceneEvent<'uiKey when 'uiKey : comparison> =
    | OpenUI of 'uiKey
    | CloseUI
    
type IScene<'nexusEvent> =
    
    abstract receive: Input -> unit
    
    abstract update: EventQueue<'nexusEvent> -> GameTime -> unit
    
    abstract draw: SpriteBatch -> unit
  

//todo: fix typegore somehow. unfortunately f# doesn't allow aliases inside definitions
  
[<AbstractClass>]
type Scene<'appState, 'uiState, 'nexusEvent, 'appEvent, 'uiEvent, 'uiKey when 'uiKey : comparison>(initialState: 'appState) =
    
    let mutable eventQueue: EventQueue<GameEvent<'nexusEvent, SceneEvent<'uiKey>, 'appEvent, 'uiEvent>> = EventQueue()
    let mutable currentState: 'appState = initialState
    let mutable uiMap: Map<'uiKey, SceneUI<'appState, 'uiState, 'nexusEvent, SceneEvent<'uiKey>, 'appEvent, 'uiEvent>> = Map.empty
    let mutable ui: SceneUI<'appState, 'uiState, 'nexusEvent, SceneEvent<'uiKey>, 'appEvent, 'uiEvent> option = None
    
    //todo: should be event, but leads to the horrible generic type mess that was just fixed. find another way if possible
    member this.addUI(key: 'uiKey)(ui: SceneUI<'appState, 'uiState, 'nexusEvent, SceneEvent<'uiKey>, 'appEvent, 'uiEvent>) =
        uiMap <- uiMap |> Map.add(key)(ui)
        
    member this.hasUI() =
        ui |> Option.isSome
          
    member this.pushSceneEvent(event: SceneEvent<'uiKey>) =
        eventQueue.push(SceneEvent(event))
    
    member this.pushAppEvent(event: 'appEvent) =
        eventQueue.push(AppEvent(event))
    
    abstract update: GameTime -> 'appState -> 'appState
    
    abstract handleInput: Input -> 'appState -> 'appState
    
    abstract handleEvent: 'appEvent -> 'appState -> 'appState
    
    abstract uiLocksInput: 'appState -> bool
    default this.uiLocksInput(_: 'appState) = true
    
    //todo: needs some initializing function in case it doesn't get overridden. also signature, wtf
    abstract chooseUI: 
        'uiKey ->
        Map<'uiKey, SceneUI<'appState, 'uiState, 'nexusEvent, SceneEvent<'uiKey>, 'appEvent, 'uiEvent>> ->
        'appState ->
        SceneUI<'appState, 'uiState, 'nexusEvent, SceneEvent<'uiKey>, 'appEvent, 'uiEvent> option
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
            
            match ui |> Option.map(fun ui -> ui.handleInput(input)) with
            | Some _ when this.uiLocksInput(currentState) -> ()
            | _ -> currentState <- currentState |> this.handleInput(input)
            
            
        override this.draw(spriteBatch) =
            this.render(spriteBatch)(currentState)
            match ui with
            | Some(ui) -> ui.render(spriteBatch)
            | None -> ()
    