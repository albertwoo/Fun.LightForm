[<AutoOpen>]
module rec Fun.LightForm.FormUtils

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


let getFormFieldValue field =
    match field.Value with
    | Valid value
    | Invalid (value, _) -> value


/// Generate form by a record
let inline generateFormByValue<'T> (value: 'T): LightForm =
    let rec loop ty value =
      FSharpType.GetRecordFields ty
      |> Seq.toList
      |> List.map (fun p ->
          if FSharpType.IsRecord p.PropertyType then
            getRecordFieldValue p.Name value
            |> loop p.PropertyType 
            |> List.map (fun f -> { f with Name = sprintf "%s.%s" p.Name f.Name })
          else
            [
              {
                Name = p.Name
                Value = Valid (getRecordFieldValue p.Name value)
              }
            ])
      |> List.concat
    loop typeof<'T> value


let inline generateFromByValueForm (form: LightValueForm<'Value>): LightForm =
    form.Value
    |> generateFormByValue
    |> List.map (fun f ->
      { f with
          Value =
            form.Errors
            |> Map.tryFind f.Name
            |> function
              | None -> f.Value
              | Some es -> Invalid(getFormFieldValue f, es) })


let generateValueForm value =
  { Value = value
    Errors = Map.empty }


/// Generate record from form based on an default record. It will modify the record itself underline.
let inline generateValueByForm<'T> (defaultValue: 'T) (form: LightForm): 'T =
    let rec loop ty defaultValue form =
      FSharpType.GetRecordFields ty
      |> Seq.toList
      |> List.iter (fun p ->
          if FSharpType.IsRecord p.PropertyType then
              let prefix = sprintf "%s." p.Name
              form
              |> List.choose (fun x ->
                  if x.Name.StartsWith prefix then Some { x with Name = x.Name.Substring(prefix.Length) }
                  else None)
              |> loop p.PropertyType (getRecordFieldValue  p.Name defaultValue)
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
    loop (defaultValue.GetType()) defaultValue form
    defaultValue


/// Generate record from form
let inline tryGenerateValueByForm<'T> (form: LightForm): Result<'T, exn> =
    let rec loop ty form =
      let values =
        FSharpType.GetRecordFields ty
        |> Seq.toList
        |> List.map (fun p ->
            if FSharpType.IsRecord p.PropertyType then
                let prefix = sprintf "%s." p.Name
                [
                  form
                  |> List.choose (fun x ->
                      if x.Name.StartsWith prefix then Some { x with Name = x.Name.Substring(prefix.Length) }
                      else None)
                  |> loop p.PropertyType
                ]
            else
                form
                |> List.tryFind (fun f -> f.Name = p.Name)
                |> function
                  | Some f -> [ getFormFieldValue f ]
                  | None   -> [])
        |> List.concat
        |> List.toArray
      FSharpValue.MakeRecord (ty, values)

    try loop typeof<'T> form |> unbox |> Ok
    with ex -> ex |> Error


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
    | LightFormMsg.ChangeField (key, value) ->
        form
        |> List.map (fun field -> 
            if field.Name <> key then field
            else
              { field with
                  Value =
                    match validateFormValue validators field value with
                    | [] -> Valid value
                    | es -> Invalid (value, es) })
    | LightFormMsg.OnFieldError (k, e) ->
        form
        |> List.map (fun field ->
            if field.Name <> k then field
            else
              { field with
                  Value =
                    match field.Value with
                    | Valid x         -> Invalid (x, [e])
                    | Invalid (x, es) -> Invalid (x, e::es) })


let getFormErrors (form: LightForm) =
    form
    |> List.map (fun field ->
        match field.Value with
        | Valid _ -> []
        | Invalid (_, es) -> es)
    |> List.concat


let getFromErrorMap (form: LightForm) =
  form
  |> List.choose (fun x ->
    match x.Value with
    | FieldValue.Invalid (_, es) -> Some(x.Name, es)
    | FieldValue.Valid _ -> None)
  |> Map.ofList


let inline updateValueFrom<'Value> (form: LightValueForm<'Value>) updator =
  let formFields = form |> generateFromByValueForm |> updator
  let errors = getFromErrorMap formFields
  match tryGenerateValueByForm<'Value> formFields with
  | Ok f ->
    { Value = f
      Errors = errors }
  | Error _ ->
    { form with Errors = errors }

let inline updateValueFormWithValidators<'Value> validators (form: LightValueForm<'Value>) = updateValueFrom form (updateFormWithValidators validators)

let inline updateValueFormWithMsg<'Value> validators msg (form: LightValueForm<'Value>) = updateValueFrom form (updateFormWithMsg validators msg)
