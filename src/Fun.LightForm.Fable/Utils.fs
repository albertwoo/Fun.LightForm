module rec Fun.LightForm.Utils

open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection
open Thoth
open Domain


let getValidationMsgs (translations: Map<string, string>) =
    translations
    |> Map.partition (fun k v -> k.StartsWith("Validation.")) 
    |> fst


let generateFieldBuilder(name, defaultValue) =
  { Name = name
    Value = defaultValue
    ValidationState = Valid }



let inline getField (fieldName: string) (o: Json.JsonValue) = o?(fieldName)

let rec generateFormByValue ty value =
    FSharpType.GetRecordFields ty
    |> Seq.toList
    |> List.map (fun p ->
        if FSharpType.IsRecord p.PropertyType then
          getField p.Name value
          |> generateFormByValue p.PropertyType 
          |> List.map (fun f -> { f with Name = sprintf "%s:%s" p.Name f.Name })
        else
          [ generateFieldBuilder (p.Name, getField p.Name value) ])
    |> List.concat


//let rec toJsonForFrom ty (model: Model) =
//    FSharpType.GetRecordFields ty
//    |> Seq.toList
//    |> List.map (fun p ->
//        if FSharpType.IsRecord p.PropertyType then
//            model
//            |> List.choose (fun x ->
//                let prefix = sprintf "%s:" p.Name
//                if x.Name.StartsWith prefix then Some ({ x with Name = x.Name.Substring(prefix.Length) })
//                else None)
//            |> toJsonForFrom p.PropertyType
//            |> Result.map (fun v -> p.Name, unbox v)
//        else
//            model
//            |> List.tryFind (fun x -> x.Name = p.Name)
//            |> function
//              | Some f -> f.Name, f.Value
//              | None   -> p.Name, box 1
//            |> Ok)
//    |> List.fold (fun s v ->
//          match s, v with
//          | Ok x, Ok v    -> Ok (v::x)
//          | Ok x, Error e -> Error e
//          | Error e, _    -> Error e
//        )
//        (Ok[])
//    |> Result.bind (
//        Json.Encode.object
//        >> Json.Encode.toString 4
//        >> Json.Decode.Auto.fromString)
