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
            //let rule = rules |> Seq.head
            for rule in rules do
                let prop = ProvidedTypeDefinition(rule.Name,baseType = Some baseTy)
                let Descs = rule.Descriptions
                prop.AddMember(ProvidedProperty(propertyName = "Description", 
                                    propertyType = typeof<string list>, 
                                    GetterCode = fun args -> <@@ Descs @@>))

                let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ obj() @@>)
                prop.AddMember(ctor)
                let ctor2 = ProvidedConstructor([ProvidedParameter("Execute", typeof<unit->float option>)], InvokeCode = fun args -> <@@ (%%(args.[0]):unit->float option) :> obj @@>)
                prop.AddMember(ctor2)

                let innerState = ProvidedProperty("Execute", typeof<unit->float option>,
                                    GetterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> unit->float option @@>)
                prop.AddMember(innerState)



                // Add documentation to the constructor
                //prop.AddMember ctor
                ty.AddMember prop

            ty
          | _ -> failwith "unexpected parameter values")) 

    do this.AddNamespace(rootNamespace, [codexTy])

[<TypeProviderAssembly>]
do ()