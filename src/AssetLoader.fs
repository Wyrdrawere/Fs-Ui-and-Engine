namespace Engine

open System
open System.IO
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics

module AssetLoader =
    let firaFont = null
    
    //todo: find out if you really want option for all this
    let mutable contentManager: ContentManager option = None
    
    //todo: build maptype that couples tryFind with loading logic
    let mutable images: Map<string, Texture2D> = Map.empty
    let mutable models: Map<string, Model> = Map.empty
    
    let loadImage(path: string) =
        match images.TryFind(path) with
        | Some(_) -> ()
        | None ->
            match contentManager with
            | Some(cm) ->
                images <- images.Add(path, cm.Load<Texture2D>(path))
            | None -> ()
            
    let loadModel(path: string) =
        // important
        // _.Load(path) automatically sets directory as Content and ends with .xnb
        // if the path is like "\Content\models\cube.xnb" then it's bad because
        // it would be "\Content\Content\models\cube.xnb.xnb"
        match models.TryFind(path) with
        | Some(_) -> ()
        | None ->
            match contentManager with
            | Some(cm) -> models <- models.Add(path, cm.Load<Model>(path))
            | None -> raise(Exception("no cm"))
            
    let setContentManager(cm: ContentManager) =
        contentManager <- Some cm
    
    let loadInitial() = 
        match contentManager with
        | Some(cm) ->
            for path in Directory.GetFiles(cm.RootDirectory + "\models") do
                //loadModel(model.Replace(".xnb", "").Replace("Content", ""))
                //models <- models.Add(path, cm.Load<Model>(path))
                //Console.WriteLine(path + "           model loaded")  
                //Console.WriteLine (cm.RootDirectory + "\models")
                loadModel (String.Format("models\{0}", Path.GetFileName(path).Replace(".xnb", "")))
            //Directory.GetFiles(cm.RootDirectory + "\models") 
            //|> Array.map Path.GetFileName
            //|> Array.iter loadModel
            //|> Array.iter (printfn "%s")
        | None -> raise(Exception("no cm in loadinitial"))