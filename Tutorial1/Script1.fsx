open System.IO

#r "../packages/FSharp.Data.2.2.0/lib/net40/FSharp.Data.dll"
open FSharp.Data

let Di = new DirectoryInfo("C:\\CodexTyranids\\images")
let Files = Di.GetFiles()

let GetFullPathWithoutExt (fileInfo:FileInfo) =
    Path.GetFileNameWithoutExtension(Path.GetFullPath(fileInfo.FullName))
    
let Jpegs = 
    Files
    |> Array.filter(fun fileInfo -> fileInfo.Length < 60L*1024L && fileInfo.Extension.Equals(".jpeg"))
    |> Array.map GetFullPathWithoutExt
    |> Set


let Jsons = 
    Files
    |> Array.filter(fun fileinfo -> fileinfo.Extension.Equals(".json") )
    |> Array.map GetFullPathWithoutExt
    |> Set
let FilesAndNames = 
    Set.difference Jpegs Jsons 
    |> Set.map(fun fileName -> fileName + ".jpeg")
    |> Set.map(fun fileName -> fileName, File.ReadAllBytes(Path.Combine(Di.FullName, fileName)))

//let FirstFileName, FireFileBytes = FilesAndNames |> Array.last
let Key = File.ReadAllText("C:\\az.txt")

let headers = [|("Content-Type", "application/octet-stream");("Ocp-Apim-Subscription-Key", Key);|] |> Array.toSeq
//
//async { let! html = Http.AsyncRequestString("https://api.projectoxford.ai/vision/v1/ocr?language=en&detectOrientation =false", httpMethod = "POST", headers = headers, body = BinaryUpload FireFileBytes)
//        match html with
//        | "{\"language\":\"en\",\"orientation\":\"NotDetected\",\"regions\":[]}" -> printfn "None"
//        | _ -> printfn "%s" html    }
//|> Async.Start

let fileWriteWithAsync (name,html,dir) = 
    // create a stream to write to
    let jsonName = Path.Combine(dir, Path.GetFileNameWithoutExtension(name) + ".json")
    printfn "%s" jsonName |> ignore
    use stream = new System.IO.FileStream(jsonName,System.IO.FileMode.Create)
    let asyncResult = stream.BeginWrite(html,0,html.Length,null,null)
    // create an async wrapper around an IAsyncResult
    let async = Async.AwaitIAsyncResult(asyncResult) |> Async.Ignore
    // block on the timer now by waiting for the async to complete
    Async.RunSynchronously async 

let findBlankHtml (name:string, html:string) = 
    match html with
    | "{\"language\":\"en\",\"orientation\":\"NotDetected\",\"regions\":[]}" -> None
    | _ -> Some (name, html)

let fetchAsync(name, imageContent) =
    let html = Http.RequestString("https://api.projectoxford.ai/vision/v1/ocr?language=en&detectOrientation =false", httpMethod = "POST", headers = headers, body = BinaryUpload imageContent)
    name,html



let runAll =
    FilesAndNames
    |> Seq.map fetchAsync 
    |> Seq.choose findBlankHtml 
    |> Seq.map (fun (x, y) -> (x, System.Text.Encoding.ASCII.GetBytes(y), Di.FullName))
    |> Seq.map fileWriteWithAsync

for i in runAll do
    System.Threading.Thread.Sleep(1000*120)
