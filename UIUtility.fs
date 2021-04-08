namespace Engine

namespace Engine.UI

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Box =
    { x: int
      y: int
      width: int
      height: int }
    with
    member this.contains(mouseState: MouseState) =
        mouseState.X >= this.x
        && mouseState.X <= this.x + this.width
        && mouseState.Y >= this.y
        && mouseState.Y <= this.y + this.height
    static member Initial =
        { x = 0
          y = 0
          width = 0
          height = 0 }
        
type Tree<'a> =
    | Leaf of 'a
    | Node of 'a * List<Tree<'a>>
    with
    member this.Content =
        match this with
        | Leaf(content) -> content
        | Node(content, _) -> content
        
module Tree =
    let rec map(f: 'a -> 'b)(tree: Tree<'a>) =
        match tree with
        | Leaf(a) -> Leaf(f a)
        | Node(a, ts) ->
            Node(f a, ts |> List.map(map f))

type Input =
    { keyboard: KeyboardState option
      mouse: MouseState option }

type Event<'appEvent, 'uiEvent> =
    | SetDelay
    | AppEvent of 'appEvent
    | UIEvent of 'uiEvent
    

type EventQueue<'event>() =
    let mutable queue: 'event List = List.empty
    
    member this.push(event: 'event) = queue <- event :: queue
    
    member this.read() = List.rev queue

module DrawPrimitive =

    let rectangle(spriteBatch: SpriteBatch)(color: Color, box: Box) =
        let tex = new Texture2D(spriteBatch.GraphicsDevice, 1, 1)
        tex.SetData([| color |])
        spriteBatch.Draw
            (tex,
             Rectangle
                 (box.x, box.y, box.width, box.height), Color.White)

    let borderedRectangle(spriteBatch: SpriteBatch)
        (rectangleColor: Color, borderColor: Color, borderSize: int, box: Box) =
            let innerRectangle =
                { x = box.x + borderSize
                  y = box.y + borderSize
                  width = box.width - 2 * borderSize
                  height = box.height - 2 * borderSize }                
            rectangle(spriteBatch)(borderColor, box)
            rectangle(spriteBatch)(rectangleColor, innerRectangle)
           
    let diamond(spriteBatch: SpriteBatch)(color: Color, box: Box) =
        let tex = new Texture2D(spriteBatch.GraphicsDevice, 1, 1)
        tex.SetData([| color |])
        spriteBatch.Draw
            (tex,
             Vector2(float32 box.x, float32 box.y),
             System.Nullable<Rectangle>(Rectangle(box.x, box.y, box.width, box.height)),
             color,
             3.141f / 4.0f,
             Vector2(0.5f * float32 box.width, 0.5f * float32 box.height),
             1.0f,
             SpriteEffects.None,
             1.0f) 
                
    //todo: string scaling is a thing, include it
    let text(spriteBatch: SpriteBatch)(text: string, font: SpriteFont, color: Color, x: int, y: int) =
        spriteBatch.DrawString(font, text, Vector2(float32 x, float32 y), color)

    let image(spriteBatch: SpriteBatch)(tex: Texture2D, box: Box) =
        spriteBatch.Draw(tex, Rectangle(box.x, box.y, box.width, box.height), Color.White)
   