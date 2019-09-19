[<AutoOpen>]
module rec Fun.LightForm.Utils

#if FABLE_COMPILER
open Fable.Core.JsInterop
#endif

open Microsoft.FSharp.Reflection


let inline private getRecordFieldValue (fieldName: string) (o: obj) =
#if FABLE_COMPILER
  o?(fieldName)
#else
    let prop = o.GetType().GetProperty fieldName
    prop.GetValue o
#endif

let inline private setRecordFieldValue (fieldName: string) (o: obj) value =
#if FABLE_COMPILER
    o?(fieldName) <- value
#else
    let prop = o.GetType().GetProperty fieldName
    prop.SetValue(o, value)
#endif


type UnionCaseValue<'T> =
  | DefaultValue of 'T
  | Values of 'T list
module UnionCase =
  let inline unboxCaseValue<'T> (value: obj[]) =
      match FSharpType.IsTuple typeof<'T> with
      | true  -> value |> unbox<'T>
      | false -> value |> Seq.head |> unbox<'T>

  /// Get all the mached cases by some case with defualt value.
  /// If nothing find then a list with default value will be returned
  let inline caseValues<'T, 'U> (targetCase: 'U) (cases: 'U seq) =
      let caseInfo, caseValue = FSharpValue.GetUnionFields(targetCase, targetCase.GetType())
      cases
      |> Seq.choose (fun c ->
          match FSharpValue.GetUnionFields(c, targetCase.GetType()) with
          | (info, v) when caseInfo.Name = info.Name -> Some v
          | _ -> None)
      |> Seq.toList
      |> function
        | [] -> UnionCaseValue.DefaultValue (unboxCaseValue<'T> caseValue)
        | x  -> UnionCaseValue.Values (x |> List.map unboxCaseValue<'T>)

  /// Will return default value or the last value in the Values
  let value (caseValue: UnionCaseValue<_>) =
      match caseValue with
      | DefaultValue v -> v
      | Values vs -> vs |> List.last

  /// Will return default value or the last value in the Values
  let values (caseValue: UnionCaseValue<_>) =
      match caseValue with
      | DefaultValue v -> [ v ]
      | Values vs      -> vs

  let concatValues (caseValue: UnionCaseValue<_>) = values caseValue |> List.concat

  /// Default value will be considered as None, for Values case will return the last value
  let toOption (caseValue: UnionCaseValue<_>) =
      match caseValue with
      | DefaultValue _ -> None
      | Values vs -> vs |> List.tryLast
      
  /// Default value will be considered as None, for Values will be put into Some
  let toOptions (caseValue: UnionCaseValue<_>) =
      match caseValue with
      | DefaultValue _ -> None
      | Values vs      -> Some vs


let getFormFieldValue field =
    match field.Value with
    | Valid value
    | Invalid (value, _) -> value


/// Generate form by a record
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


/// Generate record from form based on an default record. It will modify the record itself underline.
let rec generateValueByForm ty defaultValue (form: LightForm) =
    FSharpType.GetRecordFields ty
    |> Seq.toList
    |> List.iter (fun p ->
        if FSharpType.IsRecord p.PropertyType then
            let prefix = sprintf "%s:" p.Name
            form
            |> List.choose (fun x ->
                if x.Name.StartsWith prefix then Some { x with Name = x.Name.Substring(prefix.Length) }
                else None)
            |> generateValueByForm p.PropertyType (getRecordFieldValue  p.Name defaultValue)
            |> setRecordFieldValue p.Name defaultValue
        else
            form
            |> List.tryFind (fun f -> f.Name = p.Name)
            |> function
              | Some f ->
                  getFormFieldValue f
                  |> setRecordFieldValue p.Name defaultValue
              | None ->
                  ())
    defaultValue


let validateFormValue (validators: Map<FieldKey, Validator list>) field value =
    validators
    |> Map.filter (fun k _ -> k = field.Name)
    |> Map.toList
    |> List.map snd
    |> List.concat
    |> List.fold
        (fun s validate ->
            try match validate { field with Value = Valid value } with
                | Ok _    -> s
                | Error x -> s@x
            with ex -> s@[ex.Message])
        []


let updateFormWithValidators validators (form: LightForm) =
    form
    |> List.map (fun field ->
        { field with
            Value =
              let value = getFormFieldValue field
              match validateFormValue validators field value with
              | [] -> Valid value
              | es -> Invalid (value, es) })


let updateFormWithMsg (validators: Map<FieldKey, Validator list>) (msg: LightFormMsg) (form: LightForm): LightForm =
    match msg with
    | ChangeField (key, value) ->
        form
        |> List.map (fun field -> 
            if field.Name = key then
                { field with
                    Value =
                      match validateFormValue validators field value with
                      | [] -> Valid value
                      | es -> Invalid (value, es) }
            else field)


let getFormErrors (form: LightForm) =
    form
    |> List.map (fun field ->
        match field.Value with
        | Valid _ -> []
        | Invalid (_, es) -> es)
    |> List.concat
