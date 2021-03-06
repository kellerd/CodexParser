﻿#r "../../packages/HtmlAgilityPack/lib/Net45/HtmlAgilityPack.dll"

//open FSharp.Data
open System.IO
open System
open HtmlAgilityPack

type HtmlNode with

    member x.FollowingSibling name =
        let sibling = x.NextSibling
        if sibling = null then
            null
        elif sibling.Name = name then
            sibling
        else
            sibling.FollowingSibling name

    member x.FollowingSiblings name = seq {
        let sibling = x.NextSibling
        if sibling <> null then
            if sibling.Name = name then
                yield sibling
            yield! sibling.FollowingSiblings name
    }

    member x.PrecedingSibling name =
        let sibling = x.PreviousSibling
        if sibling = null then
            null
        elif sibling.Name = name then
            sibling
        else
            sibling.PrecedingSibling name

    member x.PrecedingSiblings name = seq {
        let sibling = x.PreviousSibling
        if sibling <> null then
            if sibling.Name = name then
                yield sibling
            yield! sibling.PrecedingSiblings name
    }

let parent (node : HtmlNode) =
    node.ParentNode

let element name (node : HtmlNode) =
    node.Element name

let elements name (node : HtmlNode) =
    node.Elements name

let descendants name (node : HtmlNode) =
    node.Descendants name

let descendantsAndSelf name (node : HtmlNode) =
    node.DescendantsAndSelf name

let ancestors name (node : HtmlNode) =
    node.Ancestors name

let ancestorsAndSelf name (node : HtmlNode) =
    node.AncestorsAndSelf name

let followingSibling name (node : HtmlNode) =
    node.FollowingSibling name

let followingSiblings name (node : HtmlNode) =
    node.FollowingSiblings name

let precedingSibling name (node : HtmlNode) =
    node.PrecedingSibling name

let precedingSiblings name (node : HtmlNode) =
    node.PrecedingSiblings name

let inline innerText (node : HtmlNode) =
    node.InnerText

let inline attr name (node : HtmlNode) =
    node.GetAttributeValue(name, "")

let inline (?) (node : HtmlNode) name =
    attr name node

let inline hasAttr name value node =
    attr name node = value

let inline hasId value node =
    hasAttr "id" value node

let inline hasClass value node =
    hasAttr "class" value node

let inline notHasClass value node =
    hasAttr "class" value node |> not

let rec nextSiblings (node : HtmlNode) = 
    seq {
        if node.NextSibling <> null then 
            yield node.NextSibling
            yield! nextSiblings(node.NextSibling)
    }

let inline hasText value (node : HtmlNode) =
    node.InnerText = value

let createDoc html = 
    let doc = new HtmlDocument()
    doc.LoadHtml html
    doc.DocumentNode

let PadForEpub pageNumber = pageNumber.ToString().PadLeft(4, '0')

type Rule = {Name : string; ReferenceImage : Uri option; Decription : string list}

let LoadEpubPages numbers asyncStringReader = 
    numbers
    |> List.map PadForEpub
    |> List.map asyncStringReader
    |> Async.Parallel
    |> Async.RunSynchronously

let TyranidsLoadFile (paddedNumber:string) = async {
    return "C:\\CodexTyranids\\text\\part" + paddedNumber + ".html" |> File.ReadAllText
    }

let ArmyRules = [17 .. 18]
let TyranidArmyPages = [19 .. 41]
let MeleeWeapons = [43]
let RangedWeapons = [44]
let BioMorph = [45]
let Artifacts = [46]
let Powers = [47]
let Schema = [48]
let Wargear = [49]
let UnitSpecs = [51 .. 82]
let Stats = [83]
let Gloassary = [84]


let extractImageOrText (node:HtmlNode) =
    match node with
        | sib when sib.Name = "div" -> sib |> descendants "img" |> Seq.head |> attr "src"
        | _ -> node.InnerText

let ParseTyranidGlossary doc = 
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
            |> Seq.map extractImageOrText
        )
    )

let rules = (Gloassary, TyranidsLoadFile) ||> LoadEpubPages |> Seq.map ParseTyranidGlossary |> Seq.collect(fun x -> x) |> Seq.toArray


for (r, desc) in rules do
    r |> printfn "%s"
    for d in desc do
        printfn "\t%s" d
