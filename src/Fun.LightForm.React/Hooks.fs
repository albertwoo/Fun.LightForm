namespace Fun.LightForm

open Fable.React
open Fun.LightForm
open Fun.LightForm.Utils


type LightFormHookBundle<'T, 'Error> =
    { Form: LightForm<'Error>
      GetValue: unit -> 'T
      GetFieldValue: FieldKey -> obj option
      Errors: 'Error list
      CreateField: FieldKey -> FieldRenderer<'Error, ReactElement> -> ReactElement }


type LightFormsHookBundle<'T, 'Error, 'Key when 'Key: comparison> =
    { Forms: ('Key * LightForm<'Error>) list
      GetValues: unit -> 'T list
      Errors: 'Error list
      CreateField: 'Key -> FieldKey -> FieldRenderer<'Error, ReactElement> -> ReactElement }



type LightForm =
    static member inline useLightForm<'Value, 'Error> (?initValue: 'Value, ?initValidators: Map<FieldKey, Validator<'Error> list>): LightFormHookBundle<'Value, 'Error> * ('Value * Map<FieldKey, Validator<'Error> list> -> unit) =
        let initLoaded = Hooks.useState false
        let form = Hooks.useState []
        let validators = Hooks.useState (initValidators |> Option.defaultValue Map.empty)
        let error = Hooks.useState []
        let changedValue = Hooks.useState None

        let setLightForm (value: 'Value, validators: Map<FieldKey, Validator<'Error> list>) =
            let newForm = value |> generateFormByRecord |> updateFormWithValidators validators
            let newError = getFormErrors newForm
            form.update newForm
            error.update newError
            changedValue.update None            

        let dispatch msg =
            let newForm = updateFormWithMsg validators.current msg form.current
            let newError = getFormErrors newForm
            form.update newForm
            error.update newError
            changedValue.update None

        let generateValue () =
            let value = generateRecordByForm form.current
            changedValue.update (Some value)
            value

        Hooks.useEffect
            (fun () ->
                match initValue, initLoaded.current with
                | Some value, false ->
                    setLightForm(value, validators.current)
                    initLoaded.update true
                | _ ->
                    ()
            ,[||])

        {
            Form = form.current
            GetValue = fun () ->
                match changedValue.current with
                | None -> generateValue()
                | Some x -> x
            GetFieldValue = fun k -> form.current |> List.tryFind (fun x -> x.Name = k) |> Option.map (fun x -> x.RawValue)
            Errors = error.current
            CreateField = 
                fun key renderer ->
                    form.current
                    |> List.tryFind (fun x -> x.Name = key)
                    |> Option.map (fun field -> renderer field dispatch)
                    |> Option.defaultWith (fun () -> str $"No field found for {key}")
        }
        , setLightForm


    static member inline useLightForm (value: 'Value, ?validators, ?dependencies): LightFormHookBundle<'Value, 'Error> =
        let validators = validators |> Option.defaultValue Map.empty
        let lightForm, setLightForm = LightForm.useLightForm<'Value, 'Error>(value, validators)

        Hooks.useEffect
            (fun () ->
                setLightForm(value, validators)
            ,dependencies |> Option.defaultValue [||])
        
        lightForm


    static member inline useLightForms<'Value, 'Error, 'Key when 'Key : comparison> (values: 'Value list, getKey: 'Value -> 'Key, ?validators, ?dependencies): LightFormsHookBundle<'Value, 'Error, 'Key> =
        let getValidators key = 
            validators
            |> Option.map (fun f -> f key)
            |> Option.defaultValue Map.empty
        
        let createForms() =
            values
            |> List.map (fun v ->
                let key = getKey v
                let form = generateFormByRecord v |> updateFormWithValidators (getValidators key)
                key, form)

        let getErrors (forms: ('Key * LightForm<_>) seq) =
            forms
            |> Seq.map (snd >> getFormErrors)
            |> Seq.concat
            |> Seq.toList
        
        let initLoaded = Hooks.useState false
        let forms = Hooks.useStateLazy createForms
        let errors = Hooks.useState []
        let changedValues = Hooks.useState []
                        
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
            let values = forms.current |> List.map (snd >> generateRecordByForm<'Value, 'Error>)
            changedValues.update values
            values

        Hooks.useEffect
            (fun () ->
                if initLoaded.current then
                    let newForms = createForms()
                    newForms |> forms.update
                    newForms |> getErrors |> errors.update
                else
                    initLoaded.update true
            ,dependencies |> Option.defaultValue [||])
        
        {
            Forms = forms.current
            GetValues = fun () ->
                if changedValues.current.Length = 0 then generateValues()
                else changedValues.current             
            Errors = errors.current
            CreateField =
                fun key field renderer ->
                    match forms.current |> List.tryFind (fun (k, _) -> k = key) with
                    | Some (_, form) ->
                        form
                        |> List.tryFind (fun x -> x.Name = field)
                        |> Option.map (fun field -> renderer field (dispatch key))
                        |> Option.defaultWith (fun () -> str $"No field found for {field}")
                    | None -> 
                        str "Create field failed because no form found"
        }
