module Fun.LightForm.Utils

#if FABLE_COMPILER
open Fable.Core.JsInterop
#endif

open Microsoft.FSharp.Reflection
open Fun.LightForm.NestFrom


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



let inline generateFormByRecord (value: 'Record) =
    let rec loop ty value =
        FSharpType.GetRecordFields ty
        |> Seq.toList
        |> List.map (fun p ->
            if FSharpType.IsRecord p.PropertyType then
                getRecordFieldValue p.Name value
                |> loop p.PropertyType 
                |> List.map (fun f -> { f with Name = p.Name <.> f.Name })
            else
                [
                    {
                        Name = p.Name
                        Value = FieldValue.Valid (getRecordFieldValue p.Name value)
                    }
                ])
        |> List.concat

    loop typeof<'Record> value |> unbox<LightForm<'Error>>


let inline generateRecordByForm<'Record, 'Error> (form: LightForm<'Error>) =
    let rec loop ty formFields =
        let values =
            FSharpType.GetRecordFields ty
            |> Array.map (fun p ->
                if FSharpType.IsRecord p.PropertyType then
                    let prefix = Spliter + p.Name
                    formFields
                    |> List.choose (fun x ->
                        if x.Name.StartsWith prefix then Some { x with Name = x.Name.Substring(prefix.Length) }
                        else None)
                    |> loop p.PropertyType
                else
                    formFields
                    |> List.tryFind (fun f -> f.Name = p.Name)
                    |> function
                        | Some f -> f.RawValue
                        | None   -> null)
        FSharpValue.MakeRecord (ty, values)

    loop typeof<'Record> form |> unbox<'Record>


let updateFormWithValidators validators (form: LightForm<'Error>) =
    form
    |> List.map (fun field ->
        { field with
            Value =
              match Validation.validateField validators field field.RawValue with
              | [] -> FieldValue.Valid field.RawValue
              | es -> FieldValue.Invalid (field.RawValue, es) })


let updateFormWithMsg (validators: Map<FieldKey, Validator<'Error> list>) (msg: LightFormMsg<'Error>) (form: LightForm<'Error>) =
    match msg with
    | LightFormMsg.ChangeField (key, value) ->
        form
        |> List.map (fun field -> 
            if field.Name <> key then field
            else
                { field with
                    Value =
                        match Validation.validateField validators field value with
                        | [] -> FieldValue.Valid value
                        | es -> FieldValue.Invalid (value, es) })
    | LightFormMsg.OnFieldError (k, e) ->
        form
        |> List.map (fun field ->
            if field.Name <> k then field
            else
                { field with
                    Value =
                        match field.Value with
                        | FieldValue.Valid x         -> FieldValue.Invalid (x, [e])
                        | FieldValue.Invalid (x, es) -> FieldValue.Invalid (x, e::es) })


let getFormErrors (form: LightForm<'Error>) =
    form
    |> List.map (fun field ->
        match field.Value with
        | FieldValue.Valid _ -> []
        | FieldValue.Invalid (_, es) -> es)
    |> List.concat


let getFormErrorsMap (form: LightForm<'Error>) =
    form
    |> List.choose (fun x ->
        match x.Value with
        | FieldValue.Invalid (_, es) -> Some(x.Name, es)
        | FieldValue.Valid _ -> None)
    |> Map.ofList
