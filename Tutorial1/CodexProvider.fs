﻿// Copyright (c) Microsoft Corporation 2005-2011.
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
//            // Create an instance of the regular expression. 
//            //
//            // This will fail with System.ArgumentException if the regular expression is invalid. 
//            // The exception will excape the type provider and be reported in client code.
//            let r = System.Text.RegularExpressions.Regex(pattern)            

            let ty = ProvidedTypeDefinition(
                        thisAssembly, 
                        rootNamespace, 
                        typeName, 
                        baseType = Some baseTy)
//
            ty.AddXmlDoc "Rules defined in the file '%s'"
            

            let rules = [filepath] |> LoadEpubPages |> Seq.collect ParseSixthEditionGlossary
            //let rule = rules |> Seq.head
            for rule in rules do
                let prop = ProvidedTypeDefinition(rule.Name,baseType = Some baseTy)
                rule.Description |> List.iteri (fun ind item -> 
                    prop.AddMember(ProvidedProperty(propertyName = "Description" + ind.ToString(), 
                                    propertyType = typeof<string>, 
                                    GetterCode = fun args -> <@@ item :> string @@>))
                )
                let Descs = rule.Description
                prop.AddMember(ProvidedProperty(propertyName = "Description", 
                                    propertyType = typeof<string list>, 
                                    GetterCode = fun args -> <@@ Descs @@>))
                let ctor = ProvidedConstructor([] 
                        ,InvokeCode = fun args -> <@@ () @@>
                        )

                // Add documentation to the constructor
                ctor.AddXmlDoc "Initializes a codex parse"
                prop.AddXmlDoc(sprintf @"Gets the rule ""%s"" for this codex " rule.Name)
                prop.AddMember ctor
                ty.AddMember prop

                    
//            // Provide strongly typed version of Codex.IsMatch static method
//            let isMatch = ProvidedMethod(
//                            methodName = "IsMatch", 
//                            parameters = [ProvidedParameter("input", typeof<string>)], 
//                            returnType = typeof<bool>, 
//                            IsStaticMethod = true,
//                            InvokeCode = fun args -> <@@ Regex.IsMatch(%%args.[0], pattern) @@>) 
//
//            isMatch.AddXmlDoc "Indicates whether the regular expression finds a match in the specified input string"
//
//            ty.AddMember isMatch
//
//            // Provided type for matches
//            // Again, erase to obj even though the representation will always be a Match
//            let matchTy = ProvidedTypeDefinition(
//                            rule.Name, 
//                            baseType = Some baseTy)
//
//            // Nest the match type within parameterized Codex type
//            ty.AddMember matchTy
//        
//            // Add group properties to match type
//            for group in r.GetGroupNames() do
//                // ignore the group named 0, which represents all input
//                if group <> "0" then
//                    let prop = ProvidedProperty(
//                                propertyName = group, 
//                                propertyType = typeof<Group>, 
//                                GetterCode = fun args -> <@@ ((%%args.[0]:obj) :?> Match).Groups.[group] @@>)
//                    prop.AddXmlDoc(sprintf @"Gets the ""%s"" group from this match" group)
//                    matchTy.AddMember(prop)
//
//            // Provide strongly typed version of Codex.Match instance method
//            let matchMeth = ProvidedMethod(
//                                methodName = "Match", 
//                                parameters = [ProvidedParameter("input", typeof<string>)], 
//                                returnType = matchTy, 
//                                InvokeCode = fun args -> <@@ ((%%args.[0]:obj) :?> Regex).Match(%%args.[1]) :> obj @@>)
//            matchMeth.AddXmlDoc "Searches the specified input string for the first occurence of this regular expression"
//            
//            ty.AddMember matchMeth
//            
            // Declare a constructor
//            let ctor = ProvidedConstructor(
//                        parameters = [] 
//                        ,InvokeCode = fun args -> <@@ () @@>
//                        )
//
//            // Add documentation to the constructor
//            ctor.AddXmlDoc "Initializes a codex parse"
//
//            ty.AddMember ctor
//            
            ty
          | _ -> failwith "unexpected parameter values")) 

    do this.AddNamespace(rootNamespace, [codexTy])

[<TypeProviderAssembly>]
do ()