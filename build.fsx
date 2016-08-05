// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing.NUnit3

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"
let testDir = "./test/"


// Filesets
let appReferences  =
    !! "/**/*.csproj"
      ++ "/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
        -- "*.zip"
        |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

// define test dlls
let testDlls = !! (buildDir + "/Tests.dll")
 
Target "NUnitTest" (fun _ ->
    testDlls
        |> NUnit3 id
)

// Build order
"Clean"
  ==> "Build"
  ==> "NUnitTest"
  ==> "Deploy"

// start build
RunTargetOrDefault "Build"
