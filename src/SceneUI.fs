namespace Engine

namespace Engine.State

open Engine.UI

type GameEvent<'nexusEvent, 'sceneEvent, 'appEvent, 'uiEvent> =
    | NexusEvent of 'nexusEvent
    | SceneEvent of 'sceneEvent
    | AppEvent of 'appEvent
    | UIEvent of 'uiEvent
    
//todo: naming. stateUI | sceneUI | something else
[<AbstractClass>]
type SceneUI<'appState, 'uiState, 'nexusEvent, 'sceneEvent, 'appEvent, 'uiEvent>(initialUIState: 'uiState, box: Box) =
    
    inherit UI<'appState, 'uiState, GameEvent<'nexusEvent, 'sceneEvent, 'appEvent, 'uiEvent>, 'uiEvent>(initialUIState, box) with
        
        override this.localize(gameEvent: GameEvent<_,_,_,_>) =
            match gameEvent with
            | UIEvent(event) -> Some(event)
            | _ -> None
                