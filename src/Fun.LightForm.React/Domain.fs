namespace Fun.LightForm


type FieldKey = string


[<RequireQualifiedAccess>]
type FieldValue<'Error> =
    | Valid of obj
    | Invalid of obj * 'Error list


type FormField<'Error> =
    { Name: FieldKey
      Value: FieldValue<'Error> }

    member this.RawValue =
        match this.Value with
        | FieldValue.Valid x -> x
        | FieldValue.Invalid (x, _) -> x

    member this.Errors =
        match this.Value with
        | FieldValue.Valid _ -> []
        | FieldValue.Invalid (_, errors) -> errors


type LightForm<'Error> = FormField<'Error> list


[<RequireQualifiedAccess>]
type LightFormMsg<'Error> =
    | ChangeField of FieldKey * value: obj
    | OnFieldError of FieldKey * 'Error


type Validator<'Error> = FormField<'Error> -> Result<unit, 'Error>
type Validators<'Error> = Map<FieldKey, Validator<'Error> list>

type FieldDispatcher<'Error> = LightFormMsg<'Error> -> unit
type FieldRenderer<'Error, 'ViewElement> = FormField<'Error> -> FieldDispatcher<'Error> -> 'ViewElement
