#r "../packages/HtmlAgilityPack.1.4.9/lib/Net45/HtmlAgilityPack.dll"
#load "HtmlAgilityPack.FSharp.fs"
#load "TyranidTypes.fsx"
#load "ProvidedTypes.fs"

//LoadFiles
open System
open HtmlAgilityPack
open HtmlAgilityPackFSharp
open TyranidTypes

type Rule = {Name : string; ReferenceImage : Uri option; Decription : string list}

let PadForEpub pageNumber = pageNumber.ToString().PadLeft(4, '0')

let LoadEpubPages numbers asyncStringReader = 
    numbers
    |> List.map PadForEpub
    |> List.map asyncStringReader
    |> Async.Parallel
    |> Async.RunSynchronously


let rules = (TyranidGloassary, TyranidsLoadFile) ||> LoadEpubPages |> Seq.map ParseTyranidGlossary |> Seq.collect(fun x -> x) |> Seq.toArray


for (r, desc) in rules do
    r |> printfn "%s"
    for d in desc do
        printfn "\t%s" d

//TypeProvider
open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
[<TypeProvider>]
type SampleTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()

    let namespaceName = "CodexParser.Rules"
    let thisAssembly = Assembly.GetExecutingAssembly()

    // Make one provided type, called TypeN.
    let makeOneProvidedType (n:int) = 
        let t = ProvidedTypeDefinition(thisAssembly,namespaceName,
                                       "Type" + string n,
                                       baseType = Some typeof<obj>)
        t.AddXmlDocDelayed (fun () -> sprintf "This provided type %s" ("Type" + string n))
        let staticProp = ProvidedProperty(propertyName = "StaticProperty", 
                                  propertyType = typeof<string>, 
                                  IsStatic=true,
                                  GetterCode= (fun args -> <@@ "Hello!" @@>))
        staticProp.AddXmlDocDelayed(fun () -> "This is a static property")
        t.AddMember staticProp
        let ctor = ProvidedConstructor(parameters = [ ], 
                               InvokeCode= (fun args -> <@@ "The object data" :> obj @@>))
        ctor.AddXmlDocDelayed(fun () -> "This is a constructor")

        t.AddMember ctor

        let ctor2 = ProvidedConstructor(parameters = [ ProvidedParameter("data",typeof<string>) ], 
                            InvokeCode= (fun args -> <@@ (%%(args.[0]) : string) :> obj @@>))
        t.AddMember ctor2
        let instanceProp = ProvidedProperty(propertyName = "InstanceProperty", 
                             propertyType = typeof<int>, 
                             GetterCode= (fun args -> 
                                               <@@ ((%%(args.[0]) : obj) :?> string).Length @@>))
        instanceProp.AddXmlDocDelayed(fun () -> "This is an instance property")
        t.AddMember instanceProp
        let instanceMeth = ProvidedMethod(methodName = "InstanceMethod", 
                               parameters = [ProvidedParameter("x",typeof<int>)], 
                               returnType = typeof<char>, 
                               InvokeCode = (fun args -> 
                                   <@@ ((%%(args.[0]) : obj) :?> string).Chars(%%(args.[1]) : int) @@>))

        instanceMeth.AddXmlDocDelayed(fun () -> "This is an instance method")
        // Add the instance method to the type.
        t.AddMember instanceMeth 
        t.AddMembersDelayed(fun () -> 
        let nestedType = ProvidedTypeDefinition("NestedType",
                                            Some typeof<obj>)

        nestedType.AddMembersDelayed (fun () -> 
            let staticPropsInNestedType = 
                [ for i in 1 .. 100 do
                        let valueOfTheProperty = "I am string "  + string i

                        let p = ProvidedProperty(propertyName = "StaticProperty" + string i, 
                                                propertyType = typeof<string>, 
                                                IsStatic=true,
                                                GetterCode= (fun args -> <@@ valueOfTheProperty @@>))

                        p.AddXmlDocDelayed(fun () -> 
                            sprintf "This is StaticProperty%d on NestedType" i)

                        yield p ]
            staticPropsInNestedType)

        [nestedType])

        // The result of makeOneProvidedType is the type.
        t
    // Now generate 100 types
    let types = [ for i in 1 .. 100 -> makeOneProvidedType i ] 

    // And add them to the namespace
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>] 
do()

let data1 = 