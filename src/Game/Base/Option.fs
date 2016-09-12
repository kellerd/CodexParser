namespace Microsoft.FSharp.Collections
module Option = 
    let either success failure opt = 
        match opt with
        | Some _ -> Option.bind success opt
        | None -> failure