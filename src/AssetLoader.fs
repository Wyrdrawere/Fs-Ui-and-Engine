namespace Engine

open System
open System.IO
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics

type Path = string

type FileSystemObject =
    | File of Path
    | Files of Path list
    | Directory of Path
    | DirectoryWindows of Path

module AssetLoader =
    let mutable defaultFont = ""
    let mutable contentManager: ContentManager option = None
    let mutable images: Map<Path, Texture2D> = Map.empty
    let mutable models: Map<Path, Model> = Map.empty
    let mutable fonts: Map<Path, SpriteFont> = Map.empty
    
    // if / won't work, duplicate the function for \ and add DirectoryWindows type to FileSystemObject
    let private loadDirectory<'filetype>(cm: ContentManager, path: Path) =
        Directory.GetFiles(cm.RootDirectory + """/""" + path)
        |> Array.toList
        |> List.map(fun filePath ->
            ( filePath, 
              cm.Load<'filetype>(String.Format(path + "/{0}", Path.GetFileName(filePath).Replace(".xnb", ""))))
            )
    let private loadDirectoryWindows<'filetype>(cm: ContentManager, path: Path) =
        Directory.GetFiles(cm.RootDirectory + """\""" + path)
        |> Array.toList
        |> List.map(fun filePath ->
            ( filePath, 
              cm.Load<'filetype>(String.Format(path + "/{0}", Path.GetFileName(filePath).Replace(".xnb", ""))))
            )
    
    let load<'filetype>(fsObject: FileSystemObject) =
        match contentManager with
        | Some(cm) ->
            let files: (string * 'filetype) list =
                match fsObject with
                | File(path) ->
                    [ (path, cm.Load<'filetype>(path)) ]
                | Files(paths) ->
                    paths |> List.map(fun path -> (path, cm.Load<'filetype>(path)))
                | Directory(path) ->
                    loadDirectory<'filetype>(cm, path)
                | DirectoryWindows(path) ->
                    loadDirectoryWindows<'filetype>(cm, path)
            for (path, file) in files do
              match box file with
              | :? Texture2D as file ->
                  images <- images.Add(path, file)
              | :? Model as file ->
                  models <- models.Add(path, file)
              | :? SpriteFont as file ->
                  fonts <- fonts.Add(path, file)
              | _ -> raise(Exception("file type not supported"))
        | None ->
            raise(Exception("no content manager"))
           
    let setContentManager(cm: ContentManager) =
        contentManager <- Some cm
        
    let setLoadDefaultFont(path: Path) =
        defaultFont <- path
        load(File path)
            
    //-remove from here-------------------------------------------------------------------------------------------------
            
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
            
    
    //todo: belongs in maingame/application from now on
    //should look like this now:
    // AssetLoader.load<Model>(Directory "models")
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