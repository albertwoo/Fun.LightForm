module rec Fun.LightForm.Validators

open System
open Fun.LightForm.Utils


let addValidations key newValidators oldValidators: Map<string, Validator list> =
    let vs =
        oldValidators
        |> Map.tryFind key
        |> Option.map (fun v -> v@newValidators)
        |> Option.defaultValue newValidators
    Map.add key vs oldValidators


let requiredAndNot errorMsg targetValue: Validator =
    fun field ->
        let value = getFormFieldValue field
        if value = null || 
           String.IsNullOrEmpty(field.Value.ToString()) ||
           value = targetValue
        then Error [ errorMsg ]
        else Ok ()

let required errorMsg = requiredAndNot errorMsg null


let maxLength errorMsg max: Validator =
    fun field ->
        if getFormFieldValue field <> null && (unbox<string>field.Value).Length > max
        then Error [ errorMsg ] 
        else Ok()

let minLength errorMsg min: Validator =
    fun field ->
        if getFormFieldValue field <> null && (unbox<string>field.Value).Length < min
        then Error [ errorMsg ]
        else Ok()


let maxNum errorMsg max: Validator =
    fun field ->
        if getFormFieldValue field <> null && unbox<float> field.Value > max
        then Error [ errorMsg ]
        else Ok()

let minNum errorMsg min: Validator =
    fun field ->
        if getFormFieldValue field <> null && unbox<float> field.Value < min
        then Error [ errorMsg ]
        else Ok()


let dateValidator errorMsg: Validator =
  fun field ->
    match getFormFieldValue field with
    | :? DateTime -> Ok()
    | x ->
        match string x |> DateTime.TryParse with
        | true, _ -> Ok()
        | false, _ -> Error [ errorMsg ]


let seqMinLen errorMsg min: Validator =
  fun field ->
    try
      if getFormFieldValue field |> unbox<_ seq> |> Seq.length < min
      then Error [ errorMsg ]
      else Ok()
    with _ ->
      Error [ "Not a sequence" ]

let seqMaxLen errorMsg max: Validator =
  fun field ->
    try
      if getFormFieldValue field |> unbox<_ seq> |> Seq.length > max
      then Error [ errorMsg ]
      else Ok()
    with _ ->
      Error [ "Not a sequence" ]
