module rec Fun.LightForm.Utils

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


let inline getRecords value = FSharpType.GetRecordFields(value.GetType())

let rec generateFormByValue value =
    FSharpType.GetRecordFields(value.GetType())
    |> Seq.toList
    |> List.map (fun p ->
        if FSharpType.IsRecord p.PropertyType then
          box 1
          |> generateFormByValue 
          |> List.map (fun f -> { f with Name = sprintf "%s:%s" p.Name f.Name })
        else
          [ generateFieldBuilder (p.Name, box 2) ])
    |> List.concat


let rec toJsonForFrom ty (model: Model) =
    FSharpType.GetRecordFields ty
    |> Seq.toList
    |> List.map (fun p ->
        if FSharpType.IsRecord p.PropertyType then
            model
            |> List.choose (fun x ->
                let prefix = sprintf "%s:" p.Name
                if x.Name.StartsWith prefix then Some ({ x with Name = x.Name.Substring(prefix.Length) })
                else None)
            |> toJsonForFrom p.PropertyType
            |> Result.map (fun v -> p.Name, unbox v)
        else
            model
            |> List.tryFind (fun x -> x.Name = p.Name)
            |> function
              | Some f -> f.Name, f.Value
              | None   -> p.Name, box 1
            |> Ok)
    |> List.fold (fun s v ->
          match s, v with
          | Ok x, Ok v    -> Ok (v::x)
          | Ok x, Error e -> Error e
          | Error e, _    -> Error e
        )
        (Ok[])
    |> Result.bind (
        Json.Encode.object
        >> Json.Encode.toString 4
        >> Json.Decode.Auto.fromString)
