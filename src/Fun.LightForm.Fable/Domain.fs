namespace rec Fun.LightForm

type LightForm = FormField list

type LightFormMsg =
    | ChangeField of FieldKey * value: obj


type FieldKey = string

type FormField =
  { Name: FieldKey
    Value: FieldValue }

type FieldValue =
  | Valid of obj
  | Invalid of obj * string list

type Validator = FormField -> Result<unit, string list>
