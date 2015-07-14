namespace CodexParser
open HtmlAgilityPack
open HtmlAgilityPackFSharp
open System.IO
open System

module ParseGlossary =
    type Rule = {Name : string; Descriptions : string list}
    let NullOrWhiteSpaceToOption a = 
        if String.IsNullOrWhiteSpace a then None else Some a
    let ParseSixthEditionGlossary doc = 
        doc 
        |> createDoc 
        |> descendants "div" 
        |> Seq.filter (hasClass "Basic-Text-Frame") 
        |> Seq.collect (fun n -> n.ChildNodes)
        |> Seq.filter (hasClass "x3-Left")
        |> Seq.map (fun node -> 
            {Name= node.InnerText;
            Descriptions = node 
                |> nextSiblings 
                |> Seq.takeWhile (notHasClass "x3-Left") 
                |> Seq.choose (fun x -> (extractImageOrText x) |> NullOrWhiteSpaceToOption )
                |> Seq.toList
            }
        )
    let LoadEpubPages path = 
        let asyncFileReader (path:string) = async {
            return path |> File.ReadAllText
        }
        path
        |> List.map asyncFileReader
        |> Async.Parallel
        |> Async.RunSynchronously
    
