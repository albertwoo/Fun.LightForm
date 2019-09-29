[<RequireQualifiedAccess>]
module Fun.LightForm.FormView.Form

open Fable.React
open Fable.React.Props
open Fun.LightForm


type FieldRenderer = FieldRenderer<Fable.React.ReactElement>


let field (state: LightForm) dispatch key (render: FieldRenderer) =
  state
  |> List.tryFind (fun x -> x.Name = key)
  |> Option.map (fun field ->
      fragment 
        [ FragmentProp.Key field.Name ] 
        [ render field dispatch ])
  |> Option.defaultValue (
      div </> [
        Style [ Color "red" ]
        Text (sprintf "Field is not configured for %s" key)
      ])


let input (props: InputProp<_> list): FieldRenderer =
    fun field dispatch ->
      inputField [
        yield! props
        InputProp.Value (field |> getFormFieldValue |> unbox)
        InputProp.OnValueChange (fun x -> ChangeField (field.Name, x) |> dispatch)
        InputProp.SimpleFieldProps [
          match field.Value with
            | Invalid (_, es) -> SimpleFieldProp.Errors es
            | Valid _ -> ()
        ]
      ]


let selector (props: SelectorProp<_, _> list): FieldRenderer =
    fun field dispatch ->
      selectorField [
        yield! props
        SelectorProp.SelectedIds (field |> getFormFieldValue |> unbox |> Seq.toList)
        SelectorProp.OnIdsChange (fun x -> ChangeField (field.Name, x) |> dispatch)
        SelectorProp.SimpleFieldProps [
          match field.Value with
            | Invalid (_, es) -> SimpleFieldProp.Errors es
            | Valid _ -> ()
        ]
      ]
