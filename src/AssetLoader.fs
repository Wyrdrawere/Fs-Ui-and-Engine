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
                  images <- images.Add(path.Substring(8).Replace(".xnb", ""), file)
              | :? Model as file ->
                  models <- models.Add(path.Substring(8).Replace(".xnb", ""), file)
              | :? SpriteFont as file ->
                  fonts <- fonts.Add(path.Substring(8).Replace(".xnb", ""), file)
              | _ -> raise(Exception("file type not supported"))
        | None ->
            raise(Exception("no content manager"))
           
    let setContentManager(cm: ContentManager) =
        contentManager <- Some cm
        
    let setLoadDefaultFont(path: Path) =
        defaultFont <- path
        load(File path)