module rec Fun.LightForm.States

open Domain


let private validate validators field =
    { field with 
        ValidationState =
            validators
            |> Map.tryFind field.Name
            |> Option.defaultValue []
            |> List.fold (fun s v ->
                try match v field with
                    | Valid     -> s
                    | Invalid x -> s@x
                with ex -> s@[ex.Message])
                []
            |> function
                | [] -> Valid
                | x -> Invalid x }


let update validators msg model =
    match msg with
    | OnChangeField (key, value) ->
        let newModel =
            model
            |> List.map (fun state -> 
                if state.Name = key then
                    { state with Value = value }
                    |> validate validators
                else state)
        newModel


let validateForm validators model =
    let newModel = model |> List.map (validate validators)
    let errors =
        newModel
        |> List.choose (fun x -> 
            match x.ValidationState with
            | Valid -> None
            | Invalid es -> Some es)
        |> List.concat
    newModel, errors
