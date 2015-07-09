namespace CodexParser
open HtmlAgilityPack
open HtmlAgilityPackFSharp
open System.IO
open System

module ParseGlossary =
    type Rule = {Name : string; Description : ImageOrText list}
    
    let ParseSixthEditionGlossary doc = 
        doc 
        |> createDoc 
        |> descendants "div" 
        |> Seq.filter (hasClass "Basic-Text-Frame") 
        |> Seq.collect (fun n -> n.ChildNodes)
        |> Seq.filter (hasClass "x3-Left")
        |> Seq.map (fun node -> 
            {Name= node.InnerText;
            Description = node 
                |> nextSiblings 
                |> Seq.takeWhile (notHasClass "x3-Left") 
                |> Seq.map extractImageOrText 
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
    
