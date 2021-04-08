namespace Engine

namespace Engine.Utility

open System
    
module GMap =
    
    [<AbstractClass>]    
    type TypeKey() =
          
        static let mutable _ID = 0
        
        let ID: int =
            let tmp = _ID
            _ID <- _ID + 1
            printfn "%A" tmp
            tmp

        member private this.ID = ID
        
        override this.Equals that =
            match that with
            | :? TypeKey as that ->
                this.ID = that.ID
                //Object.ReferenceEquals(this, that)
            | _ -> false

        override this.GetHashCode() = hash this
        
        interface IComparable with
            override this.CompareTo that =
                match that with
                | :? TypeKey as that ->
                    match (this, that) with
                    | _ when (this.Equals that) -> 0
                    | _ when (this.ID < that.ID) -> -1
                    | _ -> 1
                | _ -> invalidArg "that" "cannot compare values of different types"

    type TypeKey<'key>() = inherit TypeKey()
        
    type GMap private (container: Map<TypeKey, obj>) =
        
        member private this.container = container
        
        static member empty = GMap(Map.empty)
        
        static member add<'value>(key: TypeKey, value: 'value)(map: GMap) =
            GMap(map.container |> Map.add(key)(value :> obj))

        static member remove<'value>(key)(map: GMap) =
            GMap(map.container |> Map.remove(key))
        
        static member get<'value>(key: TypeKey)(map: GMap) =
            map.container
            |> Map.tryFind(key)
            |> Option.bind
                (fun value ->
                Some(value :?> 'value))
                
        static member apply<'value>(key: TypeKey, f: 'value -> 'value)(map: GMap) =
            match GMap.get(key)(map) with
            | Some(value) -> map |> GMap.add(key, f value)
            | None -> map
                
    type GMapTester() =
        
        let intKey = TypeKey<int>()
        
        let otherIntKey = TypeKey<int>()
        
        let stringKey = TypeKey<string>()
        
        let mutable gm1 = GMap.empty
        
        do gm1 <- gm1 |> GMap.add(intKey, 7)
        
        do printfn "%A" (gm1 |> GMap.get(intKey))
        do printfn "%A" (gm1 |> GMap.get(otherIntKey))
        do printfn "%A" (gm1 |> GMap.get(stringKey))
        
        do gm1 <- gm1 |> GMap.add(otherIntKey, 5)

        do printfn "%A" ""
        do printfn "%A" (gm1 |> GMap.get(intKey))
        do printfn "%A" (gm1 |> GMap.get(otherIntKey))
        do printfn "%A" (gm1 |> GMap.get(stringKey))

        do gm1 <- gm1 |> GMap.add(stringKey, "hello")

        do printfn "%A" ""
        do printfn "%A" (gm1 |> GMap.get(intKey))
        do printfn "%A" (gm1 |> GMap.get(otherIntKey))
        do printfn "%A" (gm1 |> GMap.get(stringKey))
        
        do gm1 <- gm1 |> GMap.add(intKey, 12)
        
        do printfn "%A" ""
        do printfn "%A" (gm1 |> GMap.get(intKey))
        do printfn "%A" (gm1 |> GMap.get(otherIntKey))
        do printfn "%A" (gm1 |> GMap.get(stringKey))
        
        do gm1 <- gm1 |> GMap.apply(intKey, fun i -> i + 4)
        
        do printfn "%A" ""
        do printfn "%A" (gm1 |> GMap.get(intKey))
        do printfn "%A" (gm1 |> GMap.get(otherIntKey))
        do printfn "%A" (gm1 |> GMap.get(stringKey))


        

        
        
