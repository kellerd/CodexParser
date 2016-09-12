namespace Microsoft.FSharp.Collections
module Map =
    let pickKeyOfItem item map = Map.pick (fun k ur -> if item = ur then Some k  else None) map
    let updateWithOrRemove f key map =
        let inner map v =
            match f v with
            | Some value -> map |> Map.add key value
            | None -> map |> Map.remove key
        Map.tryFind key map |> inner map       