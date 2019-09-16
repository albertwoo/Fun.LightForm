module rec Fun.LightForm.Domain


type Model = FormField list

type Msg =
    | OnChangeField of key: string * value: obj


type FormField =
  { Name : string
    Value : obj
    ValidationState : ValidationState }

type ValidationState = Valid | Invalid of string list

type Validator = FormField -> ValidationState
