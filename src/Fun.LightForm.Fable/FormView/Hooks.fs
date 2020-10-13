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
    { Froms: Map<'Key, LightForm>
      GetValue: unit -> Result<'T list, exn>
      GetError: unit -> string list
      HasError: unit -> bool
      CreateField: 'Key -> FieldKey -> FieldRenderer<ReactElement> -> ReactElement }


type IHooks with
    member inline _.useLightForm<'Value> (value: 'Value, ?validators, ?dependencies): LightFormHookBundle<_> =
        let validators = validators |> Option.defaultValue Map.empty
        let createFrom () = value |> generateFormByValue |> updateFormWithValidators validators
        let form = Hooks.useState([])
        let dispatch msg = updateFormWithMsg validators msg form.current |> form.update

        Hooks.useEffect
            (fun () -> createFrom() |> form.update
            ,dependencies |> Option.defaultValue [||])
        
        {
            From = form.current
            GetValue = fun () -> form.current |> tryGenerateValueByForm<'Value>
            GetFieldValue = fun k -> form.current |> List.tryFind (fun x -> x.Name = k) |> Option.map getFormFieldValue
            GetError = fun () -> form.current |> getFormErrors
            HasError = fun () -> form.current |> getFormErrors |> Seq.isEmpty |> not
            CreateField = fun key renderer -> Form.field form.current dispatch key renderer
        }


    member inline _.useLightForms<'Value, 'Key when 'Key : comparison> (values: 'Value list, getKey: 'Value -> 'Key, ?validators, ?dependencies): LightFormsHookBundle<_, _> =
        let getValidators key = validators |> Option.map (fun f -> f key) |> Option.defaultValue Map.empty
        
        let createForms() =
            values
            |> List.map (fun v ->
                let key = getKey v
                key
                ,generateFormByValue v |> updateFormWithValidators (getValidators key))
            |> Map.ofList   
        
        let forms = Hooks.useState(Map.empty)
        
        let dispatch key msg =
            forms.current
            |> Map.map (fun k f ->
                if k = key then updateFormWithMsg (getValidators key) msg f
                else f)
            |> forms.update
        
        let getValue () =
            let values = 
                forms.current
                |> Map.toList
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

            result

        let getError () =
            forms.current
            |> Map.toSeq
            |> Seq.map (snd >> getFormErrors)
            |> Seq.concat
            |> Seq.toList

        Hooks.useEffect
            (fun () -> createForms() |> forms.update
            ,dependencies |> Option.defaultValue [||])
        
        {
            Froms = forms.current
            GetValue = getValue
            GetError = getError
            HasError = fun () -> getError () |> Seq.isEmpty |> not
            CreateField = fun siteNr key renderer ->
                match forms.current |> Map.tryFind siteNr with
                | Some form -> Form.field form (dispatch siteNr) key renderer
                | None -> str "Create field failed because no form found"
        }

