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
    let baseTy = typeof<obj>
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

            let rules = [filepath] |> LoadEpubPages |> Seq.collect ParseSixthEditionGlossary
            for (name, descriptions) in rules do
                let prop = ProvidedTypeDefinition(name,baseType = Some typeof<ExecutedRule<obj>>)
                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ ExecutedRule<unit>(name,descriptions) @@>)
                prop.AddMember(ctor)
                let ctor2 = ProvidedConstructor([ProvidedParameter("Execute", typeof<obj>)], InvokeCode = fun args -> <@@ ExecutedRule(name,descriptions, Some %%args.[0]) @@>)
                prop.AddMember(ctor2)
                ty.AddMember prop

            ty
          | _ -> failwith "unexpected parameter values")) 

    do this.AddNamespace(rootNamespace, [codexTy])

[<TypeProviderAssembly>]
do ()