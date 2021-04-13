namespace Engine

namespace Engine.UI

open Engine
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Element =
    abstract LayoutNode: Tree<LayoutParameters> 
    abstract draw: SpriteBatch -> Tree<Box> -> Unit

    //todo: copies parts of draw. at least make draw use this then, if it's really necessary to have it
    //todo: does more than copy unfortunately, but there has to be something to make this less absurd/out of place
    abstract passBox: Tree<Box> -> Unit
    
module Element =

    //todo: find better way to do this (GMap might do it)
    //as of now, these settings are all distinct and only map to one specific basic element.
    //as long as this is true, combination elements work well with this, otherwise some split/merge needs to happen 
    //todo: the case above has happened, some settings now work on multiple elements, captions are fucked now
    //todo: with new widgets this might not be a bad thing, captions can be done there.
    //todo: fill in fitting/missing settings in the elements, then this is done
    type Settings =
        { width: int
          height: int
          sizeModeX: SizeMode
          sizeModeY: SizeMode
          spacingY: int
          paddingY: int
          paddingX: int
          spacingX: int
          alignmentX: Alignment
          alignmentY: Alignment
          border: int
          orientation: Orientation
          backgroundColor: Color //todo: split colors from layout settings. maybe rework this whole thing
          borderColor: Color
          textColor: Color
          font: string //path to font
          scaleX: float32 //todo: find way to make scale work with fontsize, then entire elementtrees can theoretically be scaled
          scaleY: float32 }
        
    let mutable DefaultSettings =
        { width = 0
          height = 0
          sizeModeX = Fixed
          sizeModeY = Fixed
          paddingY = 2
          paddingX = 2
          spacingX = 2
          spacingY = 2
          alignmentX = Center
          alignmentY = Center
          border = 2
          orientation = Horizontal
          backgroundColor = Color.Blue
          borderColor = Color.Gray
          textColor = Color.White
          font = AssetLoader.defaultFont
          scaleX = 1.0f
          scaleY = 1.0f }
        
    //todo: maybe move this outside model. could be nice when using widgets, so module doesn't have to be opened      
    type Parameter =
        | Width of int
        | Height of int
        | SizeMode of SizeMode
        | SizeModeX of SizeMode
        | SizeModeY of SizeMode
        | Padding of int
        | PaddingX of int
        | PaddingY of int
        | Spacing of int
        | SpacingX of int
        | SpacingY of int
        | Alignment of Alignment
        | AlignmentPos of float32 * float32
        | AlignmentX of Alignment
        | AlignmentY of Alignment
        | Border of int
        | Orientation of Orientation
        | BackgroundColor of Color
        | BorderColor of Color
        | TextColor of Color
        | Font of string
        | Scale of float32
        | ScaleX of float32
        | ScaleY of float32
        
    let rec parametersToSettings(settings: Settings, parameters: Parameter List) =
        match parameters with
        | [] -> settings
        | p::ps ->
            match p with
            | Width(value) -> parametersToSettings({settings with width = value}, ps)
            | Height(value) -> parametersToSettings({settings with height = value}, ps)
            | SizeMode(value) -> parametersToSettings({settings with sizeModeX = value; sizeModeY = value}, ps)
            | SizeModeX(value) -> parametersToSettings({settings with sizeModeX = value}, ps)
            | SizeModeY(value) -> parametersToSettings({settings with sizeModeY = value}, ps)
            | Padding(value) -> parametersToSettings({settings with paddingX = value; paddingY = value}, ps)
            | PaddingX(value) -> parametersToSettings({settings with paddingX = value}, ps)
            | PaddingY(value) -> parametersToSettings({settings with paddingY = value}, ps)
            //todo: layout ignores spacing in off direction, there is really no need for X and Y versions of this
            | Spacing(value) -> parametersToSettings({settings with spacingX = value; spacingY = value}, ps)
            | SpacingX(value) -> parametersToSettings({settings with spacingX = value}, ps)
            | SpacingY(value) -> parametersToSettings({settings with spacingY = value}, ps)
            | Alignment(value) -> parametersToSettings({settings with alignmentX = value; alignmentY = value}, ps)
            | AlignmentPos(valueX, valueY) -> parametersToSettings({settings with alignmentX = Partial(valueX); alignmentY = Partial(valueY)}, ps)
            | AlignmentX(value) -> parametersToSettings({settings with alignmentX = value}, ps)
            | AlignmentY(value) -> parametersToSettings({settings with alignmentY = value}, ps)
            | Border(value) -> parametersToSettings({settings with border = value}, ps)
            | Orientation(value) -> parametersToSettings({settings with orientation = value}, ps)
            | BackgroundColor(value) -> parametersToSettings({settings with backgroundColor = value}, ps)
            | BorderColor(value) -> parametersToSettings({settings with borderColor = value}, ps)
            | TextColor(value) -> parametersToSettings({settings with textColor = value}, ps)
            | Font(value) -> parametersToSettings({settings with font = value}, ps)
            | Scale(value) -> parametersToSettings({settings with scaleX = value; scaleY = value}, ps)
            | ScaleX(value) -> parametersToSettings({settings with scaleX = value}, ps)
            | ScaleY(value) -> parametersToSettings({settings with scaleY = value}, ps)
    
    type PanelElement(settings: Settings, children: Element List) =
        interface Element with
            override this.LayoutNode =
                let parameters =
                    { xParams =
                        { size =
                            { value = settings.width
                              mode = settings.sizeModeX
                            }
                          padding = settings.paddingX
                          spacing = settings.spacingX
                          border = settings.border
                          alignment = settings.alignmentX
                        }
                      yParams =
                        { size =
                            { value = settings.height
                              mode = settings.sizeModeY
                            }
                          padding = settings.paddingY
                          spacing = settings.spacingY
                          border = settings.border
                          alignment = settings.alignmentY
                        }
                      orientation = settings.orientation
                    }
                Node(parameters, children |> List.map(fun child -> child.LayoutNode))
            override this.draw(spriteBatch: SpriteBatch)(boxTree: Tree<Box>) =
                match boxTree with
                | Leaf _ -> ()
                | Node(box, childBoxes) ->
                    DrawPrimitive.borderedRectangle
                        (spriteBatch)
                        (settings.backgroundColor,
                         settings.borderColor,
                         settings.border,
                         box)
                    for (child, box) in List.zip(children)(childBoxes) do
                        child.draw(spriteBatch)(box)
            override this.passBox(boxTree: Tree<Box>) =
                match boxTree with
                | Leaf _ -> ()
                | Node(_, childBoxes) ->
                    for (child, box) in List.zip(children)(childBoxes) do
                        child.passBox(box)
    type LabelElement(settings: Settings, text: string) =
        
        let (sizeX, sizeY) =
            match AssetLoader.fonts |> Map.tryFind(settings.font) with
            | Some(font) ->
                let stringSize = font.MeasureString(text)
                (int <| stringSize.X, int <| stringSize.Y)
            | None ->
                (0,0)
        
        interface Element with
            override this.LayoutNode =
                Leaf <|
                    { xParams =
                        { size =
                            { value = sizeX
                              mode = settings.sizeModeX
                            }
                          padding = 0
                          spacing = 0
                          border = 0
                          alignment = settings.alignmentX
                        }
                      yParams =
                        { size =
                            { value = sizeY
                              mode = settings.sizeModeY
                            }
                          padding = 0
                          spacing = 0
                          border = 0
                          alignment = settings.alignmentY
                        }
                      orientation = Horizontal
                    }
            override this.draw(spriteBatch: SpriteBatch)(boxTree: Tree<Box>) =
                match boxTree with
                | Leaf(box) ->
                    match AssetLoader.fonts |> Map.tryFind(settings.font) with
                    | Some(font) ->
                        DrawPrimitive.text(spriteBatch)(text, font, settings.textColor, box.x, box.y)
                    | None -> ()
                | Node _ -> ()
                
            override this.passBox(_: Tree<Box>) = ()
                
    type ImageElement(settings: Settings, path: string) =
        
        do AssetLoader.load<Texture2D>(File path)
        let image = AssetLoader.images.TryFind(path)
        let(sizeX, sizeY) =
            match image with
            | Some(value) -> (int <| settings.scaleX * float32 value.Width, int <| settings.scaleY * float32 value.Height)
            | None -> (0, 0)
        
        interface Element with
            override this.LayoutNode =
                Leaf <|
                    { xParams =
                        { size =
                            { value = sizeX
                              mode = Fixed
                            }
                          padding = 0
                          spacing = 0
                          border = 0
                          alignment = settings.alignmentX
                        }
                      yParams =
                        { size =
                            { value = sizeY
                              mode = Fixed
                            }
                          padding = 0
                          spacing = 0
                          border = 0
                          alignment = settings.alignmentY
                        }
                      orientation = Horizontal
                    }
            override this.draw(spriteBatch: SpriteBatch)(boxTree: Tree<Box>) =
                match (boxTree, image) with
                | (Leaf(box), Some(image)) ->
                    DrawPrimitive.image(spriteBatch)(image, box)
                | _ -> ()
                
            override this.passBox(_: Tree<Box>) = ()    
                
    type EmptyElement(settings: Settings) =
            
            interface Element with
                override this.LayoutNode =
                    Leaf <|
                        { xParams =
                            { size =
                                { value = 0
                                  mode = settings.sizeModeX
                                }
                              padding = 0
                              spacing = 0
                              border = 0
                              alignment = Center
                            }
                          yParams =
                            { size =
                                { value = 0
                                  mode = settings.sizeModeY
                                }
                              padding = 0
                              spacing = 0
                              border = 0
                              alignment = Center
                            }
                          orientation = Horizontal
                        }
                override this.draw(_: SpriteBatch)(_: Tree<Box>) = ()
                override this.passBox(boxTree: Tree<Box>) = ()
    
    let panel(parameters: Parameter List)(content: Element List) =
        PanelElement(parametersToSettings(DefaultSettings, parameters), content) :> Element
        
    let label(parameters: Parameter List)(text: string) =
        LabelElement(parametersToSettings(DefaultSettings, parameters), text) :> Element
        
    let image(parameters: Parameter List)(path: string) =
        ImageElement(parametersToSettings(DefaultSettings, parameters), path) :> Element
        
    let caption(parameters: Parameter List)(caption: string)(path: string) =
        panel parameters
            [ image parameters path
              label parameters caption ]
        
    let empty = EmptyElement(DefaultSettings) :> Element
        
    let emptyStructural(parameters: Parameter List) =
        EmptyElement(parametersToSettings(DefaultSettings, parameters)) :> Element
         
    //todo: need better way to represent cursor. maybe even object that gets passed through widgettree
    //todo: place probably found, only needs implementation now
    let withCursor(innerElement: Element) =
        { new Element with
            override this.LayoutNode = innerElement.LayoutNode
            override this.draw(spriteBatch: SpriteBatch)(boxTree: Tree<Box>) =
                let cursorBox =
                    { x = boxTree.Content.x - 10
                      y = boxTree.Content.y + (int ((float32 boxTree.Content.height) / 2.0f))
                      width = 10
                      height = 10 }
                DrawPrimitive.diamond(spriteBatch)(Color.White, cursorBox)
                innerElement.draw(spriteBatch)(boxTree)
                
            override this.passBox(boxTree: Tree<Box>) =
                innerElement.passBox(boxTree)
        }
    
module ElementTest =
    
    open Element
    
    let test(spriteBatch: SpriteBatch) =
        let element =
            panel [Padding(4); Spacing(8); Border(4); SizeModeX(Fill); AlignmentX(Stretch); AlignmentY(Minus)]
              [label [] "Hello!"
               label [] "This is a Test"
               panel [AlignmentX(Stretch); AlignmentY(Partial(0.3f)); SizeMode(Fill)]
                 [label [] "for"]
               panel [AlignmentY(Stretch)]
                 [label [] "the"]
               panel [AlignmentX(Partial(0.3f)); AlignmentY(Partial(0.7f)); SizeMode(Fill)]
                 [label [] "new"]
               panel [Orientation(Vertical); Spacing(80); Padding(20)]
                 [label [] "Layout"
                  label [] "System"
                  image [Scale(0.1f)] "hexcolor"]]
            
        let boxTree = Layout.calculate({ x = 100; y = 100; width = 600; height = 400 }, element.LayoutNode)
        element.draw(spriteBatch)(boxTree)
       
    type Direction =
        | Forward
        | Backward
        
    let rnd = System.Random()
    let xSpeed = 0.01f
    let ySpeed = 0.005f
    let mutable x = float32 <| rnd.NextDouble()
    let mutable y = float32 <| rnd.NextDouble()
    let mutable xDir = Forward
    let mutable yDir = Backward
    let X() =
        let tmp = x
        match xDir with
        | Forward -> x <- x + xSpeed
        | Backward -> x <- x - xSpeed
        if x <= 0.0f then xDir <- Forward
        if x >= 1.0f then xDir <- Backward
        tmp
    let Y() =
        let tmp = y
        match yDir with
        | Forward -> y <- y + ySpeed
        | Backward -> y <- y - ySpeed
        if y <= 0.0f then yDir <- Forward
        if y >= 1.0f then yDir <- Backward
        tmp
        
        
    let singletonTest(spriteBatch: SpriteBatch) =
        let element =
            panel [Padding(0); Spacing(0); Border(4); AlignmentX(Stretch); AlignmentY(Stretch)]
                [ panel [ AlignmentX(Partial(X())); AlignmentY(Partial(Y())); SizeMode(Fill) ] [ image [ AlignmentY(Minus); Scale(0.1f)] "hexcolor" ]
                ; panel [ Padding(0); Spacing(0); Border(4); Orientation(Vertical); AlignmentX(Stretch); AlignmentY(Stretch); SizeMode(Fill) ]
                      [ panel [ AlignmentX(Partial(X())); AlignmentY(Partial(Y())); SizeMode(Fill) ] [ image [ AlignmentY(Minus); Scale(0.1f)] "hexcolor" ]
                      ; panel [ Padding(0); Spacing(0); Border(4); AlignmentX(Stretch); AlignmentY(Stretch); SizeMode(Fill) ]
                            [ panel [ AlignmentX(Partial(X())); AlignmentY(Partial(Y())); SizeMode(Fill) ] [ image [ AlignmentY(Minus); Scale(0.1f)] "hexcolor" ]
                            ; panel [ Padding(0); Spacing(0); Border(4); Orientation(Vertical); AlignmentX(Stretch); AlignmentY(Stretch); SizeMode(Fill) ]
                                [ panel [ AlignmentX(Partial(X())); AlignmentY(Partial(Y())); SizeMode(Fill) ] [ image [ AlignmentY(Minus); Scale(0.1f)] "hexcolor" ]
                                ; panel [ Padding(0); Spacing(0); Border(4); AlignmentX(Stretch); AlignmentY(Stretch); SizeMode(Fill) ]
                                [] 
                                ]  
                            ]
                      ]  
                ]
                
        let boxTree = Layout.calculate({ x = 100; y = 100; width = 600; height = 400 }, element.LayoutNode)
        element.draw(spriteBatch)(boxTree)
        
              