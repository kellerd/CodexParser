namespace CodexParser
open HtmlAgilityPack
open HtmlAgilityPackFSharp
open System.IO
open System

module ParseGlossary =
    

    type Rule(name : string, descriptions : string list) = 
        member this.Name = name
        member this.Descriptions = descriptions 
    type Rules(rules : seq<string*Rule>) = 
        member this.Rules = dict rules

//    type ExecutedRule<'a>(name : string, descriptions : string list, Execute: 'a option) = 
//        inherit Rule(name,descriptions) 
//        member this.Execute = Execute
//        new(name : string, descriptions : string list) =
//            ExecutedRule(name, descriptions, None)
//
    type ExecutedRule<'a> = {rule : Rule; Execute: 'a option}

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
            (node.InnerText,
             node 
                |> nextSiblings 
                |> Seq.takeWhile (notHasClass "x3-Left") 
                |> Seq.choose (fun x -> (extractImageOrText x) |> NullOrWhiteSpaceToOption )
                |> Seq.toList
            )
        )
    let LoadEpubPages path = 
        let asyncFileReader (path:string) = async {
            return path |> File.ReadAllText
        }
        path
        |> List.map asyncFileReader
        |> Async.Parallel
        |> Async.RunSynchronously
    
