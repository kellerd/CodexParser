open System.IO

#r "../packages/FSharp.Data.2.2.0/lib/net40/FSharp.Data.dll"
open FSharp.Data

let Dir = new DirectoryInfo(@"C:\CodexWolves\OEBPS\image")
let Di2 = new DirectoryInfo(@"C:\CodexRulebook\OEBPS\image")
let Di3 = new DirectoryInfo(@"C:\CodexTyranids\images")
let FilesInfoSeq = seq {
    yield Dir.GetFiles() 
    yield Di2.GetFiles()
    yield Di3.GetFiles()
} 
let Files = Array.concat FilesInfoSeq

let GetFullPathWithoutExt (fileInfo:FileInfo) =
    Path.Combine(fileInfo.Directory.FullName,Path.GetFileNameWithoutExtension(fileInfo.Name))
    
let Jpegs = 
    Files
    |> Array.filter(fun fileInfo -> fileInfo.Extension.Equals(".jpg"))
    |> Array.map GetFullPathWithoutExt
    |> Set


let Jsons = 
    Files
    |> Array.filter(fun fileinfo -> fileinfo.Extension.Equals(".json") )
    |> Array.map GetFullPathWithoutExt
    |> Set
let FilesAndNames = 
    Set.difference Jpegs Jsons 
    |> Set.map(fun fileName -> fileName + ".jpg")
    |> Set.map(fun fileName -> fileName, File.ReadAllBytes(fileName))


let Key = File.ReadAllText("C:\\az.txt")

let headers = [|("Content-Type", "application/octet-stream");("Ocp-Apim-Subscription-Key", Key);|] |> Array.toSeq

let fileWriteWithAsync (name,html) = 
    // create a stream to write to
    let jsonName = Path.GetDirectoryName(name) + @"\" + Path.GetFileNameWithoutExtension(name) + ".json"
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
    try
        let html = Http.RequestString("https://api.projectoxford.ai/vision/v1/ocr?language=en&detectOrientation =false", httpMethod = "POST", headers = headers, body = BinaryUpload imageContent)
        Some (name,html)
    with e -> None



let runAll =
    FilesAndNames 
    |> Seq.choose (fun x -> 
    System.Threading.Thread.Sleep(1000*60/20) |> ignore
    fetchAsync x)
    |> Seq.choose findBlankHtml 
    |> Seq.map (fun (x, y) -> (x, System.Text.Encoding.ASCII.GetBytes(y)))


for i in  runAll do
    fileWriteWithAsync i
