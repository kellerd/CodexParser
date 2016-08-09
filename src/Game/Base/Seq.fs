namespace Microsoft.FSharp.Collections
module Seq =
    let mode seq = 
        let newSeq = seq |> Seq.groupBy id  |> Seq.map (fun (x,y) -> x, y|> Seq.length) |> Seq.sortByDescending snd
        newSeq 
        |> Seq.tryHead 
        |> Option.map (fun (value,count) -> newSeq |> Seq.takeWhile (fun (x,seqCnt) -> count = seqCnt) |> Seq.map(fst))
        |> Option.toList |> List.toSeq
        |> Seq.concat
    let maxmode def seq =  
        match seq with 
        | xs when xs = Seq.empty -> def
        | xs -> xs |> mode |> Seq.max 
    let minmode def seq =  
        match seq with 
        | xs when xs = Seq.empty -> def
        | xs -> xs |> mode |> Seq.min

