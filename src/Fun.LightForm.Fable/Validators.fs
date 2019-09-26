module rec Fun.LightForm.Validators

open System
open Fun.LightForm.Utils


let addValidators key newValidators validators: Map<string, Validator list> =
    let vs =
        validators
        |> Map.tryFind key
        |> Option.map (fun v -> v@newValidators)
        |> Option.defaultValue newValidators
    Map.add key vs validators


let requiredAndNot targetValue errorMsg: Validator =
    fun field ->
        let value = getFormFieldValue field
        if value = null || 
           String.IsNullOrEmpty(value.ToString()) ||
           value = targetValue
        then Error [ errorMsg ]
        else Ok ()

let required errorMsg = requiredAndNot null errorMsg


let maxLength max errorMsg: Validator =
    fun field ->
        let value = getFormFieldValue field
        if value <> null && (unbox<string>value).Length > max
        then Error [ errorMsg ] 
        else Ok()

let minLength min errorMsg: Validator =
    fun field ->
        let value = getFormFieldValue field
        if value <> null && (unbox<string>value).Length < min
        then Error [ errorMsg ]
        else Ok()


let maxNum max errorMsg: Validator =
    fun field ->
        let value = getFormFieldValue field
        if value <> null && unbox<float> value > max
        then Error [ errorMsg ]
        else Ok()

let minNum min errorMsg: Validator =
    fun field ->
        let value = getFormFieldValue field
        if value <> null && unbox<float> value < min
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


let seqMinLen min errorMsg: Validator =
  fun field ->
    try
      if getFormFieldValue field |> unbox<_ seq> |> Seq.length < min
      then Error [ errorMsg ]
      else Ok()
    with _ ->
      Error [ "Not a sequence" ]

let seqMaxLen max errorMsg: Validator =
  fun field ->
    try
      if getFormFieldValue field |> unbox<_ seq> |> Seq.length > max
      then Error [ errorMsg ]
      else Ok()
    with _ ->
      Error [ "Not a sequence" ]
