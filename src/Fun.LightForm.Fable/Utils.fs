[<AutoOpen>]
module rec Fun.LightForm.Utils

open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection


let inline private getRecordFieldValue (fieldName: string) (o: obj) = o?(fieldName)

let rec generateFormByValue ty value =
    FSharpType.GetRecordFields ty
    |> Seq.toList
    |> List.map (fun p ->
        if FSharpType.IsRecord p.PropertyType then
          getRecordFieldValue p.Name value
          |> generateFormByValue p.PropertyType 
          |> List.map (fun f -> { f with Name = sprintf "%s:%s" p.Name f.Name })
        else
          [
            {
              Name = p.Name
              Value = Valid (getRecordFieldValue p.Name value)
            }
          ])
    |> List.concat


let getFormFieldValue field =
    match field.Value with
    | Valid value
    | Invalid (value, _) -> value


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
