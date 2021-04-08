namespace Engine

//test

open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics

module AssetLoader =
    
    //todo: find out if you really want option for all this
    let mutable contentManager: ContentManager option = None
    
    let mutable firaFont = null
    
    //todo: build maptype that couples tryFind with loading logic
    let mutable images: Map<string, Texture2D> = Map.empty
    
    let loadImage(path: string) =
        match images.TryFind(path) with
        | Some(_) -> ()
        | None ->
            match contentManager with
            | Some(cm) ->
                images <- images.Add(path, cm.Load<Texture2D>(path))
            | None -> ()
        
    let setContentManager(cM: ContentManager) =
        contentManager <- Some cM
    
    let loadInitial() =
        match contentManager with
        | Some(cm) -> firaFont <- cm.Load<SpriteFont>("Fira")
        | None -> ()