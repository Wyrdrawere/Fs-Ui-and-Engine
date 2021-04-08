namespace Engine

namespace Engine.UI

type SizeMode =
    | Fill
    | Fixed
    | Assigned
    //todo: fill with weight missing

type Size =
    { value: int
      mode: SizeMode }
      
type Alignment =
    | Minus
    | Center
    | Plus
    | Stretch
    | Partial of float32 //represents percentage from minus to plus, bounded between 0 and 1
    //todo: partial/stretch combination(s) missing
    
type Orientation =
    | Horizontal
    | Vertical
    with
    member this.Opposite =
        match this with
        | Horizontal -> Vertical
        | Vertical -> Horizontal

type DirectionalParameters =
    { size: Size
      padding: int
      spacing: int
      border: int
      alignment: Alignment } 

type LayoutParameters =
    { xParams: DirectionalParameters
      yParams: DirectionalParameters
      orientation: Orientation }

module Layout =
    
    let private adjustMainDirection(parameters: DirectionalParameters, children: DirectionalParameters List) =
        let minSize =
            2 * (parameters.border + parameters.padding)
            + parameters.spacing * if children.IsEmpty then 0 else children.Length - 1
        let childSize =
            children
            |> List.map(fun param -> param.size.value)
            |> List.sum
        { parameters with
            size =
                { parameters.size with
                    value =
                        minSize
                        + ([childSize; parameters.size.value] |> List.max) }} //wtf, can't this be simpler/more obvious?
        
    let private adjustOffDirection(parameters: DirectionalParameters, children: DirectionalParameters List) =
        let minSize =
            2 * (parameters.border + parameters.padding)
        if children.IsEmpty
        then { parameters with size = { parameters.size with value = minSize + parameters.size.value }}
        else
            let childSize =
                children
                |> List.map(fun param -> param.size.value)
                |> List.max
            { parameters with
                size =
                    { parameters.size with
                        value =
                            minSize + ([childSize; parameters.size.value] |> List.max)}}
        
    let rec private adjustSizes(lTree: Tree<LayoutParameters>) =
        match lTree with
        | Leaf(parameters) -> Leaf(parameters)
        | Node(parameters, children) ->
            let adjustedChildren = children |> List.map(fun child -> adjustSizes(child))
            let adjustedParameters =
                match parameters.orientation with
                | Horizontal ->
                    { parameters with
                        xParams = adjustMainDirection
                                      (parameters.xParams,
                                       adjustedChildren |> List.map(fun child -> child.Content.xParams))
                        yParams = adjustOffDirection
                                      (parameters.yParams,
                                       adjustedChildren |> List.map(fun child -> child.Content.yParams)) }
                | Vertical ->
                    { parameters with
                        xParams = adjustOffDirection
                                      (parameters.xParams,
                                       adjustedChildren |> List.map(fun child -> child.Content.xParams))
                        yParams = adjustMainDirection
                                      (parameters.yParams,
                                       adjustedChildren |> List.map(fun child -> child.Content.yParams)) }
            Node(adjustedParameters, adjustedChildren)
    
    let rec private alignDirection(position: int, size: int, parameters: DirectionalParameters) =  
        match parameters.alignment with
        | Minus -> alignDirection(position, size, { parameters with alignment = Partial(0.0f) })
        | Center -> alignDirection(position, size, { parameters with alignment = Partial(0.5f) })
        | Plus -> alignDirection(position, size, { parameters with alignment = Partial(1.0f) })
        | Stretch -> position
        | Partial(ratio) -> position + int (ratio * float32 (size - parameters.size.value))
        
    let private alignInside(box: Box, parameters: LayoutParameters) =
        { x = alignDirection(box.x, box.width, parameters.xParams)
          y = alignDirection(box.y, box.height, parameters.yParams)
          width =
              match parameters.xParams.alignment with
              | Stretch -> box.width
              | _ -> parameters.xParams.size.value
          height =
              match parameters.yParams.alignment with
              | Stretch -> box.height
              | _ -> parameters.yParams.size.value }
                
    let private partitionOffDirection(availableSpace: int)(children: DirectionalParameters List) =
        children |> List.map(fun _ -> availableSpace)
                
    let private countModes(children: DirectionalParameters List) =
        children
        |> List.map(fun child -> child.size.mode)
        |> List.fold(fun (l, x, a) mode ->
            match mode with
            | Fill -> (l+1, x, a)
            | Fixed -> (l, x+1, a)
            | Assigned -> (l, x, a+1)
            )
            (0,0,0)
         
    type PartitionCase =
        | AllAssigned
        | AllFill of int
        | Mixed
        | Singleton
        //singleton might fuck up and cause problems. partially related to offAlignment
        //update: should be fixed, but cant confirm comprehensively yet
         
    let private decideCase(counts: int * int * int) =
        match counts with
        | (0,0,_) -> AllAssigned
        | (l,0,_) -> AllFill(l)
        | (l,x,a) when l+x+a=1 -> Singleton
        | _ -> Mixed
                
    let rec private partitionMainDirection(availableSpace: int)(children: DirectionalParameters List) =
        match decideCase(countModes(children)) with
        | _ when availableSpace <= 0 -> children |> List.map(fun child -> child.size.value)
        | AllAssigned -> children |> List.map(fun child -> child.size.value)
        | AllFill(l) ->
            let cellSize = availableSpace / if l > 0 then l else 1
            let mutable availableLeft = availableSpace
            match (children |> List.tryFind(fun child -> child.size.mode = Fill && child.size.value > cellSize)) with
            | Some(largestFill) ->
                children
                |> List.map(fun child ->
                    if child.size = largestFill.size
                    then
                        availableLeft <- availableLeft - largestFill.size.value
                        { child with size = { child.size with mode = Assigned } }
                    else child )
                |> partitionMainDirection(availableLeft)
            | None ->
                let mutable amount = l
                children
                |> List.map(fun child ->
                    match (child.size.mode, l) with
                    | (Fill, l) when l > 1 ->
                        availableLeft <- availableLeft - child.size.value
                        amount <- amount - 1
                        { child with size = { child.size with value = cellSize; mode = Assigned } }
                    | (Fill, l) when l = 1 ->
                        let tmpLeft = availableLeft
                        availableLeft <- 0
                        amount <- 0
                        { child with size = { child.size with value = tmpLeft; mode = Assigned} }
                    | _ -> child)
                |> partitionMainDirection(0)
        | Mixed ->
            let mutable availableLeft = availableSpace
            children
            |> List.map(fun child ->
                if child.size.mode = Fixed
                then
                    availableLeft <- availableLeft - child.size.value
                    { child with size = { child.size with mode = Assigned } }
                else child)
            |> partitionMainDirection(availableLeft)
        | Singleton -> children |> List.map (fun _ -> availableSpace)
                
    let private zipBoxes(widths: int List, heights: int List, xCorner: int, yCorner: int, parameters: LayoutParameters) =
        let mutable xAnchor = xCorner + parameters.xParams.border + parameters.xParams.padding
        let mutable yAnchor = yCorner + parameters.yParams.border + parameters.yParams.padding
        let sizes = List.zip(widths)(heights)
        match parameters.orientation with
        | Horizontal ->
            sizes
            |> List.map(fun (width, height) ->
                let tmp = xAnchor
                xAnchor <- xAnchor + width + parameters.xParams.spacing
                { x = tmp; y = yAnchor; width = width; height = height })
        | Vertical ->
            sizes
            |> List.map(fun (width, height) ->
                let tmp = yAnchor
                yAnchor <- yAnchor + height + parameters.yParams.spacing
                { x = xAnchor; y = tmp; width = width; height = height })
            
    let rec private fitPartition(boundingBox: Box, lTree: Tree<LayoutParameters>) =
        match lTree with
        | Leaf(parameters) -> Leaf(alignInside(boundingBox, parameters))
        | Node(parameters, children) ->
            let nodeBox = alignInside(boundingBox, parameters)
            let boxes =
                match parameters.orientation with
                | Horizontal ->
                    let availableWidth =
                        nodeBox.width
                        - 2 * (parameters.xParams.border + parameters.xParams.padding)
                        - parameters.xParams.spacing * if children.IsEmpty then 0 else children.Length - 1
                    let availableHeight =
                        nodeBox.height
                        - 2 * (parameters.yParams.border + parameters.yParams.padding)
                    zipBoxes
                        (partitionMainDirection(availableWidth)(children |> List.map(fun child -> child.Content.xParams)),
                         partitionOffDirection(availableHeight)(children |> List.map(fun child -> child.Content.yParams)),
                         nodeBox.x,
                         nodeBox.y,
                         parameters)
                | Vertical ->
                    let availableWidth =
                        nodeBox.width
                        - 2 * (parameters.xParams.border + parameters.xParams.padding)
                    let availableHeight =
                        nodeBox.height
                        - 2 * (parameters.yParams.border + parameters.yParams.padding)
                        - parameters.yParams.spacing * if children.IsEmpty then 0 else children.Length - 1
                    zipBoxes
                        (partitionOffDirection(availableWidth)(children |> List.map(fun child -> child.Content.xParams)),
                         partitionMainDirection(availableHeight)(children |> List.map(fun child -> child.Content.yParams)),
                         nodeBox.x,
                         nodeBox.y,
                         parameters)
            let alignedBoxes =
                List.zip(boxes)(children) |> List.map(fun (box, child) -> alignInside(box, child.Content))
            Node(nodeBox, List.zip(alignedBoxes)(children) |> List.map(fun (box, child) -> fitPartition(box, child)))
                
    let calculate(boundingBox: Box, lTree: Tree<LayoutParameters>): Tree<Box> =
        fitPartition(boundingBox, adjustSizes(lTree))
                
    //todo: make Tree<LayoutParameters> a type that can inherit hash/equal interface. why no typeclasses? :(
    let rec hashParameters(tree: Tree<LayoutParameters>) =
        match tree with
        | Leaf(content) ->
            hash content
        | Node(content, trees) ->
            hash (content, trees |> List.map(hashParameters))
            
    let measureSizes(tree: Tree<LayoutParameters>) =
        let adjusted = adjustSizes(tree)
        (adjusted.Content.xParams.size.value, adjusted.Content.yParams.size.value)
        
    let measureWidth(tree: Tree<LayoutParameters>) =
        match measureSizes(tree) with
        | (x,_) -> x
        
    let measureHeight(tree: Tree<LayoutParameters>) =
        match measureSizes(tree) with
        | (_,y) -> y