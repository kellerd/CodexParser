namespace Microsoft.FSharp.Collections
module Map =
    let pickKeyOfItem item map = Map.pick (fun k ur -> if item = ur then Some k  else None) map
    let replace f x oldKey = (Map.add oldKey (f x))