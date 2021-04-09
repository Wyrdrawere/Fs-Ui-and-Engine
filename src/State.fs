namespace Engine

namespace Engine.State

open Engine.System
open Engine.UI
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics


//todo: maybe put gameEvent here, for switching IState and other stuff
type IState =
    
    abstract update: GameTime * EventQueue<Input> -> unit
    
    abstract draw: SpriteBatch -> unit
  
  
[<AbstractClass>]    
type State<'gameEvent, 'appState, 'appEvent, 'uiState, 'uiEvent>(initialState: 'appState) =
    
    let mutable currentState: 'appState = initialState
    //todo: might not be the best way to do things. multiple, layering uis could be made, or just some that are annoying to layout otherwise
    //todo: or upgrade layout to allow for this sort of thing (layering hard, better layouting easy)
    let mutable ui: UI<'appState, 'appEvent, 'uiState, 'uiEvent> option = None
    
    abstract update: GameTime -> 'state -> 'state
    
    abstract receiveInput: EventQueue<Input> -> 'state -> 'state
    
    abstract receiveEvent: EventQueue<'appEvent> -> 'state -> 'state
    
    abstract render: SpriteBatch -> 'state -> unit
    
    interface IState with
        override this.update(gameTime: GameTime, input: EventQueue<Input>) =
            match ui with
            | Some(ui) ->
                let events = ui.update(currentState, gameTime, input)
                //todo: need some way to specify if state is allowed input even when ui exists
                currentState <- currentState |> this.update(gameTime) |> this.receiveEvent(events)
            | None ->
                currentState <- currentState |> this.update(gameTime) |> this.receiveInput(input)
            
        override this.draw(spriteBatch) =
            this.render(spriteBatch)(currentState)
            match ui with
            | Some(ui) -> ui.render(spriteBatch)
            | None -> ()
    
    