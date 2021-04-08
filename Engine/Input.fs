namespace Engine

namespace Engine.System

open Microsoft.Xna.Framework.Input

type KeyboardInput =
    | UpArrow
    | DownArrow
    | LeftArrow
    | RightArrow
    | ConfirmButton //Circle
    | CancelButton //Cross/X
    | ToggleButton //Square
    | MenuButton //Triangle
    //todo: find better names for these Buttons without directly mapping them to the controller
    
module KeyboardInput =
    let Translate(keyboard: KeyboardState) =
        let mutable keys = []
        if keyboard.IsKeyDown(Keys.Up) then keys <- UpArrow::keys
        if keyboard.IsKeyDown(Keys.Down) then keys <- DownArrow::keys
        if keyboard.IsKeyDown(Keys.Left) then keys <- LeftArrow::keys
        if keyboard.IsKeyDown(Keys.Right) then keys <- RightArrow::keys
        if keyboard.IsKeyDown(Keys.Enter) then keys <- ConfirmButton::keys
        if keyboard.IsKeyDown(Keys.Back) then keys <- CancelButton::keys
        if keyboard.IsKeyDown(Keys.RightShift) then keys <- ToggleButton::keys
        if keyboard.IsKeyDown(Keys.Space) then keys <- MenuButton::keys
        keys
    
type Input =
    { mouse: MouseState option
      keyboard: KeyboardInput list }

type Event<'s> =
    interface
    end