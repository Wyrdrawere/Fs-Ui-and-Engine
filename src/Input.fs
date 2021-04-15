namespace Engine

namespace Engine.System

open Microsoft.Xna.Framework.Input


type Input =
    | KeyPressed of Keys
    | KeyReleased of Keys
    
type EventQueue<'event>() =
    let mutable queue: 'event List = List.empty
    
    member this.push(event: 'event) = queue <- event :: queue
    
    member this.read() = List.rev queue
    
type Event<'appEvent, 'uiEvent> =
    | AppEvent of 'appEvent
    | UIEvent of 'uiEvent
    