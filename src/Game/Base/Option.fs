namespace Microsoft.FSharp.Collections
module Option = 
    let map2 f a b =
        a |> Option.bind (fun a' -> b |> Option.map (fun b' -> f a' b'))

    let either f x = function
        | None -> x
        | Some v -> f v
    let negate newValue = either (fun _ -> None) newValue