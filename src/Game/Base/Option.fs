namespace Microsoft.FSharp.Collections
module Option = 
    let map2 f a b =
        a |> Option.bind (fun a' -> b |> Option.map (fun b' -> f a' b'))
    let either success failure opt = 
        match opt with
        | Some _ -> Option.bind success opt
        | None -> failure