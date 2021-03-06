namespace Engine

namespace Engine.UI

open Engine
open Engine.System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Element
open Engine.Extensions
  
type WsBase() =
    
    member val LastBox: Box = Box.Initial with get, set
    
type IWidget<'event> =
    inherit Element
    
    abstract initBox: Tree<Box> -> Unit
    abstract initView: Unit -> Element
    abstract lastBox: Unit -> Box
    abstract update: GameTime -> EventQueue<'event> -> Unit
    abstract receive: Input -> EventQueue<'event> -> Unit
    abstract view: Unit -> Element
    
[<AbstractClass>]        
type Widget<'widgetState, 'event when 'widgetState :> WsBase>(state: 'widgetState) =
        
    
    abstract update: GameTime -> EventQueue<'event> -> Unit
    abstract receive: Input -> EventQueue<'event> -> Unit
    abstract view: Unit -> Element
    
    
    member this.asWidget() =
        this :> IWidget<'event>
        
    member this.setBox(layout: Tree<Box>) =
        state.LastBox <- layout.Content
        this.view().passBox(layout)
    abstract initBox: Tree<Box> -> Unit
    default this.initBox(tree: Tree<Box>) =
        state.LastBox <- tree.Content
    abstract initView: Unit -> Element
    default this.initView() = this.view()
    abstract layoutNode: Unit -> Tree<LayoutParameters>
    default this.layoutNode() = this.view().LayoutNode
    abstract draw: SpriteBatch -> Tree<Box> -> Unit
    default this.draw(spriteBatch: SpriteBatch)(layout: Tree<Box>) =
            state.LastBox <- layout.Content
            this.view().draw(spriteBatch)(layout)
                
    interface IWidget<'event> with
        
        override this.initBox(tree: Tree<Box>) =
            this.initBox(tree)
        override this.lastBox() =
            state.LastBox
        override this.initView() =
            this.initView()
        override this.update(gameTime: GameTime)(queue: EventQueue<'event>) =
            this.update(gameTime)(queue)
        override this.receive(input: Input)(queue: EventQueue<'event>) =
            this.receive(input)(queue)
        override this.view() =
            this :> Element
    
        override this.LayoutNode = this.layoutNode()
        override this.draw(spriteBatch: SpriteBatch)(layout: Tree<Box>) = this.draw(spriteBatch)(layout)
        override this.passBox(layout: Tree<Box>) =
            this.setBox(layout)
      
      
//todo: implementations from here. split and move to folder when replacement is done        
        
//todo: larger usability change with state parameters. coupled with element
type WsPanel<'event>() =
    
    inherit WsBase()
    
    member val Children: IWidget<'event> list = [] with get, set
    
    member val Spacing = 2 with get, set
    member val Padding = 2 with get, set
    member val BorderSize = 2 with get, set
    member val Orientation = Horizontal with get, set
    member val SizeMode = Fixed with get, set
    member val Alignment = Stretch with get, set
    member val Color = Color.Blue with get, set
    member val BorderColor = Color.Gray with get, set
    
    member this.Parameters =
        [ Spacing(this.Spacing)
          Padding(this.Padding)
          Border(this.BorderSize)
          Orientation(this.Orientation)
          SizeMode(this.SizeMode)
          Alignment(this.Alignment)
          BackgroundColor(this.Color)
          BorderColor(this.BorderColor) ]
    
    
    static member New(children: IWidget<'event> list) =
        let tmp = WsPanel()
        tmp.Children <- children
        tmp
        
type Panel<'event>(state: WsPanel<'event>) =    
    inherit Widget<WsPanel<'event>, 'event>(state) with
        
        override this.initBox(tree: Tree<Box>) =
            state.LastBox <- tree.Content
            match tree with
            | Node(_, trees) ->
                for (child, tree) in List.zip(state.Children)(trees) do
                    child.initBox(tree)
            | Leaf _ -> ()
                       
        override this.initView() =
            panel (state.Parameters) (state.Children |> List.map(fun child -> child.initView()))
                       
        override this.update(gameTime: GameTime)(queue: EventQueue<'event>) =
            for child in state.Children do
                child.update(gameTime)(queue)
            
        override this.receive(input: Input)(queue: EventQueue<'event>) =
            for child in state.Children do
                child.receive(input)(queue)
              
        override this.view() =
            panel (state.Parameters) (state.Children |> List.map(fun child -> child.view()))
         
type WsLabel() =
    
    inherit WsBase()
    
    member val Label: string = "" with get, set
    
    member val LastLength: int = 0 with get, set
    
    member val Font: string = AssetLoader.defaultFont with get, set
    member val TextColor = Color.White with get, set
    member val Alignment = Center with get, set
    member this.Parameters =
        [ Font(this.Font)
          TextColor(this.TextColor)
          Alignment(this.Alignment) ]
    
    static member New(label: string) =
        let tmp = WsLabel()
        tmp.Label <- label
        tmp.LastLength <- label.Length
        tmp
    
    
         
type Label<'event>(state: WsLabel) =    
    inherit Widget<WsLabel, 'event>(state) with
        
        override this.update(_: GameTime)(_: EventQueue<'event>) =
            let newLength = state.Label.Length
            if not (newLength = state.LastLength)
            then
                state.LastLength <- newLength
            
        override this.receive(_: Input)(_: EventQueue<'event>) = ()
            
              
        override this.view() =
            label (state.Parameters) state.Label
        
type WsImage() =
    
    inherit WsBase()
    
    member val Path: string = "hexcolor" with get, set
    
    member val Scale = 0.1f with get, set
    
    member this.Parameters =
        [ Scale(this.Scale) ]
    
    static member New(path: string) =
        let tmp = WsImage()
        tmp.Path <- path
        tmp
        
type Image<'event>(state: WsImage) =    
    inherit Widget<WsImage, 'event>(state) with
        
        override this.update(_: GameTime)(_: EventQueue<'event>) = ()
            
        override this.receive(_: Input)(_: EventQueue<'event>) = ()
              
        override this.view() =
            image state.Parameters state.Path
         
type WsLabelButton<'event>()=
    
    inherit WsBase()
    
    member val Label: string = "" with get, set
    member val OnClick: 'event list = [] with get, set
    member val Hovered: bool = false with get, set
    member val Selected: bool = false with get, set
    member val Pressed: bool = false with get, set
    
    member val Color = Color.Blue with get, set
    member val HoverColor = Color.LightBlue with get, set
    member val PressedColor = Color.DarkBlue with get, set
    member val BorderColor = Color.Gray with get, set
    member val SelectBorderColor = Color.Yellow with get, set
    member val ButtonSizeMode = Fixed with get, set
    member val ButtonAlignment = Center with get, set
    member val Padding = 2 with get, set
    member val BorderSize = 2 with get, set
    member val Font: string = AssetLoader.defaultFont with get, set
    member val TextColor = Color.White with get, set
    
    member val TextSizeModeX = Fixed with get, set
    member val TextSizeModeY = Fixed with get, set
    member val TextAlignmentX = Center with get, set
    member val TextAlignmentY = Center with get, set
    
    member this.ButtonParameters =
        let color =
            if this.Pressed
            then this.PressedColor
            else if this.Hovered
            then this.HoverColor
            else this.Color
        let borderColor =
            if this.Selected
            then this.SelectBorderColor
            else this.BorderColor
        [ BackgroundColor(color)
          BorderColor(borderColor)
          SizeMode(this.ButtonSizeMode)
          Alignment(this.ButtonAlignment)
          Padding(this.Padding)
          Border(this.BorderSize) ]
        
    member this.TextParameters =
        [ Font(this.Font)
          TextColor(this.TextColor)
          SizeModeX(this.TextSizeModeX)
          SizeModeY(this.TextSizeModeY)
          AlignmentX(this.TextAlignmentX)
          AlignmentY(this.TextAlignmentY) ]
    
    
    member this.reset() =
        this.Hovered <- false
        this.Selected <- false
        this.Pressed <- false
    
    static member New(label: string, onClick: 'event list) =
        let tmp = WsLabelButton()
        tmp.Label <- label
        tmp.OnClick <- onClick
        tmp
    
type CaptionOrder =
    | TextFirst
    | ImageFirst
                            
type WsCaptionButton<'event>() =
    
    inherit WsLabelButton<'event>()
    
    member val Path: string = "hexcolor" with get, set
    
    member val CaptionMode = TextFirst
    member val Orientation = Vertical with get, set
    member val Spacing = 2 with get, set
    member val Scale = 0.1f with get, set
    member val ImageSizeModeX = Fixed with get, set
    member val ImageSizeModeY = Fixed with get, set
    member val ImageAlignmentX = Center with get, set
    member val ImageAlignmentY = Center with get, set
    
    member this.ButtonParameters =
        base.ButtonParameters @
        [ Orientation(this.Orientation)
          Spacing(this.Spacing) ]
    
    member this.ImagePanelParameters =
        [ SizeModeX(this.ImageSizeModeX)
          SizeModeY(this.ImageSizeModeY)
          AlignmentX(this.ImageAlignmentX)
          AlignmentY(this.ImageAlignmentY)
          BackgroundColor(Color.Clear)
          BorderColor(Color.Clear)
          Border(0)
          Padding(0) ]
    
    member this.ImageParameters =
        [ Scale(this.Scale) ]
          
    
    static member New(label: string, path: string, onClick: 'event list) =
        let tmp = WsCaptionButton()
        tmp.Label <- label
        tmp.Path <- path
        tmp.OnClick <- onClick
        tmp
        
type ButtonMode<'event> =
    | LabelButton of WsLabelButton<'event>
    | CaptionButton of WsCaptionButton<'event>
    
type WsButton<'event>(buttonMode: ButtonMode<'event>) =
    inherit WsBase()
    
    member val buttonState = buttonMode with get
    
    member this.LastBox
        with get() =
            match buttonMode with
            | LabelButton(button) -> button.LastBox
            | CaptionButton(button) -> button.LastBox
        and set(value) =
            match buttonMode with
            | LabelButton(button) -> button.LastBox <- value
            | CaptionButton(button) -> button.LastBox <- value
            
    static member NewLabelButton(label: string, onClick: 'event list) =
        WsButton(LabelButton(WsLabelButton.New(label, onClick)))
        
    static member NewCaptionButton(label: string, path: string, onClick: 'event list) =
        WsButton(CaptionButton(WsCaptionButton.New(label, path, onClick)))
    
type Button<'event>(state: WsButton<'event>) =    
    inherit Widget<WsButton<'event>, 'event>(state) with
        
        override this.update(_: GameTime)(_: EventQueue<'event>) =
            ()
            
        override this.receive(input: Input)(queue: EventQueue<'event>) =
            let state =
                match state.buttonState with
                | LabelButton(button) -> button
                | CaptionButton(button) -> button :> WsLabelButton<'event>
            
            match input with
            | KeyPressed(Keys.Enter) when not (state.OnClick |> List.isEmpty) ->
                state.Pressed <- true
            | KeyReleased(Keys.Enter) ->
                if state.Pressed
                then
                    for event in state.OnClick do
                        queue.push(event)
                state.Pressed <- false
            | _ -> ()
           
            
        override this.view() =
            match state.buttonState with
            | LabelButton(state) ->    
                panel
                    state.ButtonParameters
                    [ label state.TextParameters state.Label ]
            | CaptionButton(state) ->
                let content =
                    [ label state.TextParameters state.Label
                      panel state.ImagePanelParameters [ image state.ImageParameters state.Path ] ]
                panel
                    state.ButtonParameters 
                    (match state.CaptionMode with
                     | TextFirst -> content
                     | ImageFirst -> List.rev content)
                        
type MenuInputMode =
    | Direct
    | AsPanel
        
type WsMenu<'event>() =
    
    inherit WsBase()
      
    member val Rows: int = 0 with get, set
    
    member val Cols: int = 0 with get, set
    
    member val Children: IWidget<'event> list = [] with get, set
    
    member val OffScrollDepth: int option = None with get, set
     
    member val CursorX: int = 0 with get, set
    
    member val CursorY: int = 0 with get, set
    
    member val AnchorX: int = 0 with get, set
    
    member val AnchorY: int = 0 with get, set
    
    member val LargestWidth: int = 0 with get, set
    
    member val LargestHeight: int = 0 with get, set
    
    member val InputMode: MenuInputMode = Direct with get, set
    
    member val DisplayCursor: bool = false with get, set
    
    member val Active: bool = true with get, set
    
    member val Orientation = Horizontal with get, set
    member val Spacing = 2 with get, set
    member val Padding = 2 with get, set
    member val BorderSize = 2 with get, set
    member val SizeMode = Fixed with get, set
    member val Alignment = Center with get, set
    member val Color = Color.Blue with get, set
    member val BorderColor = Color.Gray with get, set
    
    member this.Parameters =
        [ Orientation(this.Orientation)
          Spacing(this.Spacing)
          Padding(this.Padding)
          Border(this.BorderSize)
          SizeMode(this.SizeMode)
          Alignment(this.Alignment)
          BackgroundColor(this.Color)
          BorderColor(this.BorderColor) ]
    
    member this.reset() =
        this.CursorX <- 0
        this.CursorY <- 0
        this.AnchorX <- 0
        this.AnchorY <- 0
    
    member this.Index =
        this.index(this.Orientation)
    
    member this.calculateDimensions() =
        let (x,y) =
            this.Children
            |> List.map(fun child -> child.LayoutNode |> Layout.measureSizes)
            |> List.fold(fun (accX, accY) (childX, childY) ->
                ((if accX > childX then accX else childX), (if accY > childY then accY else childY)))
                (0,0)
        
        this.LargestWidth <- x
        this.LargestHeight <- y
        
        
    member this.index(orientation: Orientation) =
        match (orientation, this.OffScrollDepth) with
        | (Horizontal, Some(depth)) ->
            (this.AnchorX + this.CursorX) * depth + this.AnchorY + this.CursorY
        | (Horizontal, _) ->
            (this.AnchorX + this.CursorX) * this.Rows + this.AnchorY + this.CursorY
        | (Vertical, Some(depth)) ->
            (this.AnchorY + this.CursorY) * depth + this.AnchorX + this.CursorX
        | (Vertical, _) -> 
            (this.AnchorY + this.CursorY) * this.Cols + this.AnchorX + this.CursorX
            
    static member New(rows: int, cols: int, children: IWidget<'event> list, ?offScrollDepth: int) =
        let tmp = WsMenu()
        tmp.Rows <- rows
        tmp.Cols <- cols
        tmp.OffScrollDepth <- offScrollDepth
        tmp.Children <- children
        tmp
        
type Menu<'event>(state: WsMenu<'event>) =
    inherit Widget<WsMenu<'event>, 'event>(state) with
        
        override this.initBox(tree: Tree<Box>) =
            state.LastBox <- tree.Content
            match tree with
            | Node(_, trees) ->
                for (child, tree) in List.zip(state.Children)(trees) do
                    child.initBox(tree)
                    state.calculateDimensions()
            | Leaf _ -> ()
            
        override this.initView() =
            panel state.Parameters (state.Children |> List.map(fun child -> child.initView())) 
          
        override this.update(gameTime: GameTime)(queue: EventQueue<'event>) =
            if state.Active
            then
                for child in state.Children do
                    child.update(gameTime)(queue)
            
        override this.receive(input: Input)(queue: EventQueue<'event>) =
            let handleScroll() =
                
                let maxY =
                    match (state.Orientation, state.OffScrollDepth) with
                    | (Horizontal, Some(depth)) ->
                        depth
                    | (Vertical, Some(depth)) ->
                        int <| System.Math.Ceiling ((float state.Children.Length) / (float depth))
                    | (Horizontal, None) ->
                        state.Rows
                    | (Vertical, None) ->
                        int <| System.Math.Ceiling ((float state.Children.Length) / (float state.Cols))
                        
                let maxX =
                    match (state.Orientation, state.OffScrollDepth) with
                    | (Horizontal, Some(depth)) ->
                        int <| System.Math.Ceiling ((float state.Children.Length) / (float depth))
                    | (Vertical, Some(depth)) ->
                        depth
                    | (Horizontal, None) ->
                        int <| System.Math.Ceiling ((float state.Children.Length) / (float state.Rows))
                    | (Vertical, None) ->
                        state.Cols
                
                match input with
                | KeyPressed(Keys.Up) when state.AnchorY + state.CursorY > 0 ->
                    if state.CursorY > 0
                    then state.CursorY <- state.CursorY - 1
                    else state.AnchorY <- state.AnchorY - 1
                | KeyPressed(Keys.Down) when state.AnchorY + state.CursorY < maxY - 1 ->
                    if state.CursorY = state.Rows - 1
                    then
                        state.AnchorY <- state.AnchorY + 1
                    else
                        state.CursorY <- state.CursorY + 1
                | KeyPressed(Keys.Left) when state.AnchorX + state.CursorX > 0 ->
                    if state.CursorX > 0
                    then state.CursorX <- state.CursorX - 1
                    else state.AnchorX <- state.AnchorX - 1
                | KeyPressed(Keys.Right) when state.AnchorX + state.CursorX < maxX - 1 ->
                    if state.CursorX = state.Cols - 1
                    then
                        state.AnchorX <- state.AnchorX + 1
                    else
                        state.CursorX <- state.CursorX + 1
                | _ -> ()
                
            if state.Active
            then
                match state.InputMode with
                | Direct -> handleScroll()    
                | AsPanel -> ()
                
                state.Children
                |> List.tryItem(state.Index)
                |> Option.map(fun child -> child.receive(input)(queue))
                |> ignore
                    
            //todo: make clear which direction is scrollable
                
        override this.view() =
                
            let itemPanel(items: Element list) =
                panel
                    [ SizeMode(Fill)
                      Alignment(Stretch) 
                      Width(state.LargestWidth)
                      Height(state.LargestHeight)
                      Border(0)
                      Padding(0)
                      BackgroundColor(Color.Clear)
                      BorderColor(Color.Clear)
                      Orientation(state.Orientation.Opposite)
                      Spacing(state.Spacing)
                    ]
                    items
            let cursorIndex = state.Index
            
            let menuPanel(takeMain: int, dropMain: int, takeOff: int, dropOff: int)(children: IWidget<'event> list) =
                
                let depth =
                    match state.OffScrollDepth with
                    | Some(depth) when depth > takeOff -> depth
                    | _ -> takeOff
                
                children
                |> List.map(fun child -> itemPanel [child.view()])
                |> List.fill(emptyStructural [ SizeMode(Fill)], System.Math.roundByBase(children.Length, takeMain * depth))
                |> List.mapi(fun index elem -> if index = cursorIndex && state.DisplayCursor then withCursor(elem) else elem)
                |> List.chunkBySize(depth)
                |> List.map(fun cs -> cs |> List.skip(dropOff) |> List.take(takeOff))
                |> List.skip(dropMain)
                |> List.take(takeMain)
                |> List.map(itemPanel)
                |> panel state.Parameters
                                 
            match state.Orientation with
            | Horizontal ->
                menuPanel(state.Cols, state.AnchorX, state.Rows, state.AnchorY)(state.Children)
            | Vertical ->
                menuPanel(state.Rows, state.AnchorY, state.Cols, state.AnchorX)(state.Children)
                
//todo: do NOT use, will get removed/replaced at some point      
[<AbstractClass>]
type MenuBuilder<'input, 'event>(menuState: WsMenu<'event>) =
    
    let mutable stateCache: 'input option = None
    let mutable buttons: WsButton<'event> list = []
    
    member val MenuState = menuState with get
    
    abstract makeButtons: 'input -> WsButton<'event> list
    
    member this.build() =
        Menu(this.MenuState) :> IWidget<'event>
    
    member this.updateButtons(appState: 'input) =
        stateCache <- Some(appState)
        this.resetButtons()
        
    member this.resetButtons() =
        let newButtons =
            match stateCache with
            | Some(appState) -> this.makeButtons(appState)
            | None -> []
        this.MenuState.Children <- newButtons |> List.map(fun button -> Button(button) :> IWidget<'event>)
        
    member this.replaceAt(index: int, replacement: IWidget<'event>) =
        this.MenuState.Children <-
            this.MenuState.Children
            |> List.mapi(fun buttonIndex button ->
                if index = buttonIndex
                then replacement
                else button)
    
    member this.replaceCurrent(replacement: IWidget<'event>) =
        this.replaceAt(this.MenuState.Index, replacement)
        
        