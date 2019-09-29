[<AutoOpen>]
module Fun.LightForm.FormViews.Views

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
      div
        [ Style [ Color "red" ] ] 
        [ str (sprintf "Field is not configured for %s" key) ])



let inputF (props: InputProp<_> list): FieldRenderer =
    fun field dispatch ->
      inputField [
        yield! props
        yield InputProp.Value (field |> getFormFieldValue |> unbox)
        yield InputProp.OnValueChange (fun x -> ChangeField (field.Name, x) |> dispatch)
        yield InputProp.SimpleFieldProps [
          match field.Value with
            | Invalid (_, es) -> yield SimpleFieldProp.Errors es
            | Valid _ -> ()
        ]
      ]

let selectorF (props: SelectorProp<_, _> list): FieldRenderer =
    fun field dispatch ->
      selectorField [
        yield! props
        yield SelectorProp.SelectedIds (field |> getFormFieldValue |> unbox |> Seq.toList)
        yield SelectorProp.OnIdsChange (fun x -> ChangeField (field.Name, x) |> dispatch)
        yield SelectorProp.SimpleFieldProps [
          match field.Value with
            | Invalid (_, es) -> yield SimpleFieldProp.Errors es
            | Valid _ -> ()
        ]
      ]
