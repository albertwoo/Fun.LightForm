namespace rec Fun.LightForm


type LightForm = FormField list

/// This form state can be easier for SSR, especially when deserialize in front end
type LightValueForm<'Value> =
    { Value: 'Value
      Errors: Map<FieldKey, string list> }


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
