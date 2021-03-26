module Fun.LightForm.Validation

open System
open Fun.LightForm


let emptyValidators<'Error> = Map.empty<FieldKey, 'Error>


let addValidators key newValidators (validators: Validators<'Error>): Validators<'Error> =
    let vs =
        validators
        |> Map.tryFind key
        |> Option.map (fun v -> v@newValidators)
        |> Option.defaultValue newValidators
    Map.add key vs validators


let validate value (validators: Validator<'Error> seq) =
    let field = { Name = ""; Value = FieldValue.Valid value }
    validators
    |> Seq.fold
        (fun s validator ->
            match validator field with
            | Ok _ -> s
            | Error e -> s@[e])
        []


let validateField (validators: Validators<'Error>) field value =
    validators
    |> Map.filter (fun k _ -> k = field.Name)
    |> Map.toList
    |> List.map snd
    |> List.concat
    |> validate value


let safeCreateWithAllErrors validators map value =
    validate value validators
    |> function
        | [] -> map value |> Ok
        | errors -> errors |> Error

let safeCreateWithFirstError map validators value =
    validate value validators
    |> function
        | error::_ -> Error error
        | _ -> map value |> Ok



// ==========================================================================


let required error: Validator<'Error> =
    fun field ->
        let value = field.RawValue
        if value = null || 
           String.IsNullOrEmpty(value.ToString())
        then
            Error error
        else Ok ()

let not target error: Validator<'Error> =
    fun field ->
        let value = field.RawValue
        if target = unbox value then Error error
        else Ok()


let maxLength max error: Validator<'Error> =
    fun field ->
        let value = field.RawValue
        if value <> null && (unbox<string>value).Length > max
        then Error (error max)
        else Ok()

let minLength min error: Validator<'Error> =
    fun field ->
        let value = field.RawValue
        if value <> null && (unbox<string>value).Length < min
        then Error (error min)
        else Ok()


let maxNum max error: Validator<'Error> =
    fun field ->
        let value = field.RawValue
        if value <> null && unbox value > max
        then Error (error max)
        else Ok()

let minNum min error: Validator<'Error> =
    fun field ->
        let value = field.RawValue
        if value <> null && unbox value < min
        then Error (error min)
        else Ok()


let dateValidator error: Validator<'Error> =
    fun field ->
        match field.RawValue with
        | :? DateTime -> Ok()
        | x ->
            match string x |> DateTime.TryParse with
            | true, _ -> Ok()
            | false, _ -> Error error


let seqMinLen min error: Validator<'Error> =
    fun field ->
        if field.RawValue |> unbox |> Seq.length < min
        then Error (error min)
        else Ok()

let seqMaxLen max error: Validator<'Error> =
    fun field ->
        if field.RawValue |> unbox |> Seq.length > max
        then Error (error max)
        else Ok()


