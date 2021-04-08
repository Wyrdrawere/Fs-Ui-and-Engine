namespace Engine

namespace Engine.Utility

module Maybe =

    type MaybeBuilder() =
            member this.Bind(m, f) = Option.bind f m
            member this.Return(x) = Some(x)
            member this.Zero() = None
            
    let maybe = MaybeBuilder()
