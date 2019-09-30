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
        InputProp.OnValueChange (fun x -> LightFormMsg.ChangeField (field.Name, x) |> dispatch)
        InputProp.SimpleFieldProps [
          match field.Value with
            | Invalid (_, es) -> SimpleFieldProp.Errors es
            | Valid _ -> ()
        ]
      ]


/// If OnlyOne is not true, then Field.Value must a a squence
let selector (props: SelectorProp<_, _> list): FieldRenderer =
    fun field dispatch ->
      let onlyOne = props |> UnionProps.tryLast (function SelectorProp.OnlyOne x -> Some x | _ -> None) |> Option.defaultValue true

      let selectedIds =
        try
          let value = field |> getFormFieldValue
          if onlyOne then [ unbox value ]
          else value |> unbox |> Seq.toList
        with ex ->
          LightFormMsg.OnFieldError (field.Name, string ex) |> dispatch
          []

      selectorField [
        yield! props
        SelectorProp.SelectedIds selectedIds
        SelectorProp.OnSelect (fun x ->
          let newValue =
            if onlyOne then x |> Seq.tryHead |> box
            else x |> box
          LightFormMsg.ChangeField (field.Name, newValue) |> dispatch)
        SelectorProp.SimpleFieldProps [
          match field.Value with
            | Invalid (_, es) -> SimpleFieldProp.Errors es
            | Valid _ -> ()
        ]
      ]
