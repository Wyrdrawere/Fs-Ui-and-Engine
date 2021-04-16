namespace Engine

namespace Engine.System

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics

//todo: move MainGame in engine and make it a new type initialized by giving a nexus

//todo: functionality for loading and sound should be part of this 
type NexusEvent<'sceneKey> =
    | SetScene of 'sceneKey
    | AddScene of 'sceneKey * IScene<NexusEvent<'sceneKey>>
    | Print of string

type INexus =
    abstract receive: Input -> unit
    abstract update: GameTime -> unit
    abstract draw: SpriteBatch -> unit

type Nexus<'sceneKey when 'sceneKey : comparison>(initialScenes: Map<'sceneKey, IScene<NexusEvent<'sceneKey>>>, firstScene: 'sceneKey) =
    
    let mutable eventQueue: EventQueue<NexusEvent<'sceneKey>> = EventQueue()
    let mutable scenes: Map<'sceneKey, IScene<NexusEvent<'sceneKey>>> = initialScenes
    let mutable activeScene: 'sceneKey = firstScene
        
    member private this.manipulateScene<'a>(parameter: 'a, sceneFunction: IScene<NexusEvent<'sceneKey>> -> ('a -> unit)) =
        scenes |> Map.tryFind(activeScene)
        |> Option.map(fun scene -> sceneFunction scene parameter)
        |> ignore
    
    member this.receive(input: Input) =
        this.manipulateScene<Input>(input, fun scene -> scene.receive)
    
    member this.update(gameTime: GameTime) =
        
        this.manipulateScene<GameTime>(gameTime, fun scene -> scene.update(eventQueue))
        
        for event in eventQueue.read() do
            match event with
            | SetScene(key) -> activeScene <- key
            | AddScene(key, scene) -> scenes <- scenes |> Map.add(key)(scene)
            | Print(str) -> printfn "%A" str
            
        eventQueue <- EventQueue()
    
    member this.draw(spriteBatch) =
        this.manipulateScene<SpriteBatch>(spriteBatch, fun scene -> scene.draw)
    
    
    interface INexus with
        override this.receive(input) = this.receive(input)
        override this.update(gameTime) = this.update(gameTime)
        override this.draw(spriteBatch) = this.draw(spriteBatch)
    