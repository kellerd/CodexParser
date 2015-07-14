#r "../packages/HtmlAgilityPack.1.4.9/lib/Net45/HtmlAgilityPack.dll"
#r @".\bin\Debug\CodexParser.dll"
#load "HtmlAgilityPack.FSharp.fs"
#load "ParseGlossary.fs"
//LoadFiles
open HtmlAgilityPack
open HtmlAgilityPackFSharp
open System.IO
open CodexParser.ParseGlossary
open System
let TyranidsLoadFile (paddedNumber:string) = 
     """C:\CodexTyranids\text\part""" + paddedNumber + ".html" 

let TyranidArmyRules = [17 .. 18]
let TyranidTyranidArmyPages = [19 .. 41]
let TyranidMeleeWeapons = [43]
let TyranidRangedWeapons = [44]
let TyranidBioMorph = [45]
let TyranidArtifacts = [46]
let TyranidPowers = [47]
let TyranidSchema = [48]
let TyranidWargear = [49]
let TyranidUnitSpecs = [51 .. 82]
let TyranidStats = [83]
let TyranidGloassary = [84]

let PadForEpub pageNumber = pageNumber.ToString().PadLeft(4, '0')
let x () = 
    (fun _ -> 5)
let rules = TyranidGloassary |> List.map (fun i -> i |> PadForEpub |> TyranidsLoadFile ) |> LoadEpubPages |> Seq.collect ParseSixthEditionGlossary |> Seq.toArray
open CodexParser.CodexTypeProvider
type T = CodexTyped<path="""C:\CodexTyranids\text\part0084.html""">
let r = T.``Move Through Cover``(fun _ -> Some 8.5)
let r2 = T.``Acid maw``()
do match r.Execute() with
    | Some x -> printfn "the valid value is %f" x
    | None -> printfn "No values"
do match r2.Execute() with
    | Some x -> printfn "the valid value is %f" x
    | None -> printfn "No values"
