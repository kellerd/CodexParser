// Copyright (c) Microsoft Corporation 2005-2011.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

// This file contains a set of helper types and methods for providing types in an implementation 
// of ITypeProvider.
//
// This code is a sample for use in conjunction with the F# 3.0 Developer Preview release of September 2011.

namespace CodexParser.CodexTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Text.RegularExpressions
open CodexParser.ParseGlossary
open System.IO

[<TypeProvider>]
type public CheckedCodexProvider() as this =
    inherit TypeProviderForNamespaces()

    // Get the assembly and namespace used to house the provided types
    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "CodexParser.CodexTypeProvider"
    let baseTy = typeof<Rules>
    let staticParams = [ProvidedStaticParameter("path", typeof<string>)]
    

    let codexTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "CodexTyped", Some baseTy)
    
    do codexTy.DefineStaticParameters(
        staticParams, 
        (fun typeName parameterValues ->

          match parameterValues with 
          | [| :? string as filepath |] -> 
//          
            let ty = ProvidedTypeDefinition(
                        thisAssembly, 
                        rootNamespace, 
                        typeName, 
                        baseType = Some baseTy)

            let rules = [filepath] |> LoadEpubPages |> Seq.collect ParseSixthEditionGlossary |> Seq.map (fun (name, descriptions) -> (name, Rule(name, descriptions)))
            for name, rule in rules do
                let prop = ProvidedTypeDefinition(rule.Name,baseType = Some typeof<obj>)
                prop.AddMember(ProvidedProperty(name, typeof<string>))
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ (name) :> obj @@>)
                prop.AddMember(ctor)
                
                ty.AddMember prop
            ty.AddMember(ProvidedProperty("Rules", typeof<System.Collections.Generic.Dictionary<string,Rule>>))
            let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ Rules(rules) @@>)
            ty
          | _ -> failwith "unexpected parameter values")) 

    do this.AddNamespace(rootNamespace, [codexTy])

[<TypeProviderAssembly>]
do ()