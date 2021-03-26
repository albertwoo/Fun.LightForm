#r "nuget: Fake.Core.Process,5.20.3"
#r "nuget: Fake.IO.FileSystem,5.20.3"
#r "nuget: Fake.DotNet.Cli,5.20.3"
#r "nuget: Fake.JavaScript.Yarn,5.20.3"
#r "nuget: BlackFox.Fake.BuildTask,0.1.3"

#load "./scripts/WebClient.fsx"

open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.DotNet
open Fake.JavaScript
open BlackFox.Fake


let sample1Dir = "samples/Sample1"
let publishDir = __SOURCE_DIRECTORY__ </> "docs"


fsi.CommandLineArgs
|> Array.skip 1
|> BuildTask.setupContextFromArgv 


let checkEnv =
    BuildTask.create "CheckEnv" [] {
        Yarn.exec "--version" id
        Yarn.install (fun x -> { x with WorkingDirectory = sample1Dir </> "www" })
        DotNet.exec id "tool restore" "" |> ignore
    }

let startCientDev =
    BuildTask.create "StartClientDev" [ checkEnv ] {
        WebClient.startDev sample1Dir 8080
    }

let bundleClient =
    BuildTask.create "BundleClient" [ checkEnv ] {
        Shell.cleanDir publishDir
        WebClient.bundle sample1Dir publishDir
    }


BuildTask.runOrDefault startCientDev
