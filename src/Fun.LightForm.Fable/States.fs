[<AutoOpen>]
module rec Fun.LightForm.LightFormState

open Fun.LightForm.Utils


let private validateValue (validators: Map<FieldKey, Validator list>) field value =
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
              match validateValue validators field value with
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
                      match validateValue validators field value with
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
