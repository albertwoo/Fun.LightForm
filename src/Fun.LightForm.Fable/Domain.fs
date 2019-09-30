namespace rec Fun.LightForm


type LightForm = FormField list

[<RequireQualifiedAccess>]
type LightFormMsg =
    | ChangeField of FieldKey * value: obj
    | OnFieldError of FieldKey * string


type FormField =
  { Name: FieldKey
    Value: FieldValue }
and FieldKey = string
and FieldValue =
  | Valid of obj
  | Invalid of obj * string list


type Validator = FormField -> Result<unit, string list>


type FieldDispatcher = LightFormMsg -> unit
type FieldRenderer<'ViewElement> = FormField -> FieldDispatcher -> 'ViewElement
