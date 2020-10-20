[<AutoOpen>]
module Fun.LightForm.FormView.HooksExtensions

open Fable.React
open Fun.LightForm
open Fun.LightForm.FormView


type LightFormHookBundle<'T> =
    { From: LightForm
      GetValue: unit -> Result<'T, exn>
      GetFieldValue: FieldKey -> obj option
      GetError: unit -> string list
      HasError: unit -> bool
      CreateField: FieldKey -> FieldRenderer<ReactElement> -> ReactElement }

type LightFormsHookBundle<'T, 'Key when 'Key: comparison> =
    { Froms: ('Key * LightForm) list
      GetValue: unit -> Result<'T list, exn>
      GetError: unit -> string list
      HasError: unit -> bool
      CreateField: 'Key -> FieldKey -> FieldRenderer<ReactElement> -> ReactElement }


type IHooks with
    member inline _.useLightForm<'Value> (value: 'Value, ?validators, ?dependencies): LightFormHookBundle<_> =
        let form = Hooks.useState []
        let error = Hooks.useState []
        let changedValue = Hooks.useState None

        let validators = validators |> Option.defaultValue Map.empty

        let createFrom () = value |> generateFormByValue |> updateFormWithValidators validators

        let dispatch msg =
            let newForm = updateFormWithMsg validators msg form.current
            let newError = getFormErrors newForm
            form.update newForm
            error.update newError
            changedValue.update None

        let generateValue () =
            form.current 
            |> tryGenerateValueByForm<'Value>
            |> Result.map (fun value ->
                changedValue.update (Some value)
                value)

        Hooks.useEffect
            (fun () -> createFrom() |> form.update
            ,dependencies |> Option.defaultValue [||])
        
        {
            From = form.current
            GetValue = fun () ->
                match changedValue.current with
                | None -> generateValue()
                | Some x -> Ok x
            GetFieldValue = fun k -> form.current |> List.tryFind (fun x -> x.Name = k) |> Option.map getFormFieldValue
            GetError = fun () -> error.current
            HasError = fun () -> error.current.Length > 0
            CreateField = fun key renderer -> Form.field form.current dispatch key renderer
        }


    member inline _.useLightForms<'Value, 'Key when 'Key : comparison> (values: 'Value list, getKey: 'Value -> 'Key, ?validators, ?dependencies): LightFormsHookBundle<_, _> =
        let forms = Hooks.useState []
        let errors = Hooks.useState []
        let changedValues = Hooks.useState []

        let getValidators key = 
            validators 
            |> Option.map (fun f -> f key)
            |> Option.defaultValue Map.empty
        
        let createForms() =
            values
            |> List.map (fun v ->
                let key = getKey v
                key
                ,generateFormByValue v |> updateFormWithValidators (getValidators key))
                
        let dispatch key msg =
            let newForms =
                forms.current
                |> List.map (fun (k, f) ->
                    if k = key then k, updateFormWithMsg (getValidators key) msg f
                    else k, f)

            let newErrors =
                newForms
                |> Seq.map (snd >> getFormErrors)
                |> Seq.concat
                |> Seq.toList

            forms.update newForms
            errors.update newErrors
            changedValues.update []
        
        let generateValues () =
            let values = 
                forms.current
                |> List.map (snd >> tryGenerateValueByForm<'Value>)

            let mutable result = Ok []
            let mutable shouldContinue = true
            let mutable index = 0
            while shouldContinue && index < values.Length do
                let value = values.[index]
                index <- index + 1
                match value with
                | Ok x ->
                    result <- result |> Result.map (fun ls -> x::ls)
                | Error e ->
                    result <- Error e
                    shouldContinue <- false

            match result with
            | Ok x -> changedValues.update x
            | _ -> ()

            result

        Hooks.useEffect
            (fun () -> createForms() |> forms.update
            ,dependencies |> Option.defaultValue [||])
        
        {
            Froms = forms.current
            GetValue = fun () ->
                if changedValues.current.Length = 0 then generateValues()
                else Ok changedValues.current             
            GetError = fun () -> errors.current
            HasError = fun () -> errors.current.Length > 0
            CreateField = fun key fieldName renderer ->
                match forms.current |> List.tryFind (fun (k, _) -> k = key) with
                | Some (_, form) -> Form.field form (dispatch key) fieldName renderer
                | None -> str "Create field failed because no form found"
        }

