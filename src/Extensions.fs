namespace Engine

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module Extensions =
        
    type System.String with
        member this.ensureLengthLeft(totalWidth: int, paddingChar: char) =
            this.PadLeft(totalWidth, paddingChar).[..totalWidth-1]
        member this.ensureLengthLeft(totalWidth: int) =
            this.ensureLengthLeft(totalWidth, ' ')
        member this.ensureLengthRight(totalWidth: int, paddingChar: char) =
            this.PadRight(totalWidth, paddingChar).[..totalWidth-1]
        member this.ensureLengthRight(totalWidth: int) =
            this.ensureLengthRight(totalWidth, ' ')
            
    type Microsoft.Xna.Framework.Color with
        static member Clear = Color.FromNonPremultiplied(0,0,0,0)
    
    module List =
        
        let rec private fillHelper<'T>(element: 'T, length: int)(list: 'T List) =
            if list.Length < length
            then fillHelper(element, length)(element :: list)
            else list
                
                
        let fill<'T>(element: 'T, length: int)(list: 'T List) =
            list |> List.rev |> fillHelper(element, length) |> List.rev
        
        let rec private fillWithNewHelper<'T>(elementFunction: unit -> 'T, length)(list: 'T List) =
            if list.Length < length
            then fillWithNewHelper(elementFunction, length)(elementFunction() :: list)
            else list
        
        let fillWithNew<'T>(elementFunction: unit -> 'T, length)(list: 'T List) =
            list |> List.rev |> fillWithNewHelper(elementFunction, length) |> List.rev
    
    type System.Math with
        
        static member roundByBase(value: int, base_: int) =
            if not (value / base_ = 0)
            then value + (base_ - value % base_)  
            else
                if value > base_
                then value
                else base_