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
open System.Linq
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
let rules = TyranidGloassary |> List.map (fun i -> i |> PadForEpub |> TyranidsLoadFile ) |> LoadEpubPages |> Seq.collect ParseSixthEditionGlossary |> Seq.map (fun (name, descriptions) -> (name))

open CodexParser.CodexTypeProvider
[<Literal>]
let filepath = """C:\CodexTyranids\text\part0084.html"""
type TyranidsRules = CodexTyped<filepath>
let u = TyranidsRules.``Acid blood``.Description
let r = TyranidsRules.Rules
for i in r do
    printfn "%s" i

type Model = {Name:string; BS:int}
type Actions =
    | Assault of TyranidsRules.Assault * (Model * Model->Model)
    | Hit of TyranidsRules.``Preferred Enemy`` * Model
TyranidsRules.``Preferred Enemy``.Description
//let x = T.``Acid blood``(fun () -> 8.5 * (System.DateTime.Now.Ticks |> Convert.ToDouble))
//let y = T.``Adaptive Biology``(fun () -> 8.5 * (System.DateTime.Now.Ticks |> Convert.ToDouble))
//let z = T.Assault(fun (a, b) -> a * b)
//let a = T.``Bio-plasmic cannon``(fun a b -> a + b)
//
//
//let b = {rule=T.``Acid blood``(); Execute = Some (fun () -> 8.5 * (System.DateTime.Now.Ticks |> Convert.ToDouble))}
//
//do match z.Execute with
//    | Some a -> (3,3) |> (a :?> (int*int->int)) |> printfn "the valid value is %d"
//    | None -> printfn "No values"
//    
//do match z.Execute with
//    | Some a -> (a :?> (int*int->int))(3, 3)|> printfn "the valid value is %d"
//    | None -> printfn "No values"
//    
//do match y.Execute with
//    | Some a -> (a :?> (unit->float))() |> printfn "the valid value is %f"
//    | None -> printfn "No values"
//    
//do match a.Execute with
//    | Some a -> (a :?> (int->int->int)) 3 3 |> printfn "the valid value is %d"
//    | None -> printfn "No values"
//
//do match u.Execute with
//    | Some a -> a :?> unit |> ignore
//    | None -> printfn "No values"
