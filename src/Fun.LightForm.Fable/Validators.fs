module rec Fun.LightForm.Validators

open System
open Domain


let addValidations key newValidators oldValidators: Map<string, Validator list> =
    let vs =
        oldValidators
        |> Map.tryFind key
        |> Option.map (fun v -> v@newValidators)
        |> Option.defaultValue newValidators
    Map.add key vs oldValidators


let required errorMsg: Validator =
    fun field ->
        if field.Value = null || 
           String.IsNullOrEmpty(field.Value.ToString())
        then Invalid [ errorMsg ]
        else Valid

let requiredAndNot errorMsg value: Validator =
    fun field ->
        required errorMsg { field with Value = if field.Value = box value then null else box 1 }


let maxLength errorMsg max: Validator =
    fun field ->
        if field.Value <> null && (unbox<string>field.Value).Length > max
        then Invalid [ errorMsg ] 
        else Valid

let minLength errorMsg min : Validator =
    fun field ->
        if field.Value <> null && (unbox<string>field.Value).Length < min
        then Invalid [ errorMsg ]
        else Valid


let maxNum errorMsg max: Validator =
    fun field ->
        if field.Value <> null && unbox<float> field.Value > max
        then Invalid [ errorMsg ]
        else Valid

let minNum errorMsg min: Validator =
    fun field ->
        if field.Value <> null && unbox<float> field.Value < min
        then Invalid [ errorMsg ]
        else Valid
