module Fun.LightForm.FormViews

open System
open Fable.React
open Fable.React.Props


type FieldRenderer = FieldRenderer<Fable.React.ReactElement>


let Classes = String.concat " " >> Class


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


let fieldOuter classes children =
  div
    [
      match classes with
        | [] -> yield Style [ Margin "5px"; Padding "5px" ]
        | cs -> yield Classes cs
    ]
    children


let fieldLabel classes label =
  div
    [
      match classes with
        | [] -> yield Style [ Color "gray"; FontSize "0.9rem" ]
        | cs -> yield Classes cs
    ]
    [ str label ]


let fieldError classes fieldValue =
    match fieldValue with
    | Valid _ -> []
    | Invalid (_, es) ->
         [
            for e in es ->
              div
                [
                  match classes with
                    | [] -> yield Style [ Color "red"; FontSize "0.8rem"; Opacity "0.8" ]
                    | cs -> yield Classes cs
                ]
                [ str e ]
         ]


type InputProp =
  | InputType of InputType
  | Label of string
  | OuterClasses of string list
  | InputClasses of string list
  | LabelClasses of string list
  | ErrorClasses of string list
  | InputViewWrapper of (ReactElement -> ReactElement)
  | InputAttrs of IHTMLProp list
and InputType =
  | Text
  | Email
  | Number
  | Password
  | Date of format: string

let inputField (props: InputProp list): FieldRenderer =
    fun field dispatch ->

      let label             = props |> UnionProps.tryLast (function InputProp.Label x -> Some x | _ -> None)
      let inputType         = props |> UnionProps.tryLast (function InputProp.InputType x -> Some x | _ -> None) |> Option.defaultValue InputType.Text
      let inputViewWrapper  = props |> UnionProps.tryLast (function InputProp.InputViewWrapper x -> Some x | _ -> None)
      let inputAttrs        = props |> UnionProps.concat (function InputProp.InputAttrs x -> Some x | _ -> None)
      let outerClasses      = props |> UnionProps.concat (function InputProp.OuterClasses x -> Some x | _ -> None)
      let labelClasses      = props |> UnionProps.concat (function InputProp.LabelClasses x -> Some x | _ -> None)
      let inputClasses      = props |> UnionProps.concat (function InputProp.InputClasses x -> Some x | _ -> None)
      let errorClasses      = props |> UnionProps.concat (function InputProp.ErrorClasses x -> Some x | _ -> None)

      let inputView = 
        input [
          yield! inputAttrs
          match inputClasses with
            | [] -> yield Style [ Width "100%"; Margin "2px 0"; Padding "2px 5px"; BackgroundColor "#f1f1f1" ]
            | cs -> yield Classes cs
          yield Type (
            match inputType with
              | Text      -> "text"
              | Email     -> "email"
              | Date _    -> "date"
              | Number    -> "number"
              | Password  -> "password")
          yield DefaultValue (
            let value = field |> getFormFieldValue
            match inputType with
              | Text
              | Email
              | Password
              | Number -> if isNull value then box "" else box value
              | Date f ->
                  try (value :?> DateTime).ToString(f) |> box
                  with _ -> box value)
          yield OnChange (fun e ->
            match inputType with
              | Text
              | Email
              | Password
              | Number
              | Date _ -> ChangeField (field.Name, e.Value)
            |> dispatch)
        ]

      fieldOuter outerClasses
        [
          if label.IsSome then yield fieldLabel labelClasses label.Value 

          match inputViewWrapper with
            | Some wrapper -> yield wrapper inputView
            | None         -> yield inputView

          yield! fieldError errorClasses field.Value 
        ]


type SelectorProp<'Id, 'Value> =
  | SourceList of ('Id * 'Value) list
  | Displayer of ('Id * 'Value -> ReactElement)
  | Label of string
  | OuterClasses of string list
  | LabelClasses of string list
  | ErrorClasses of string list
  | InputClasses of string list
  | InputAttrs of IHTMLProp list

let inline selectorField<'id, 'v when 'id : equality> (props: SelectorProp<'id, 'v> list): FieldRenderer =
    fun field disp ->

      let label         = props |> UnionProps.tryLast (function SelectorProp.Label x -> Some x | _ -> None)
      let sourceList    = props |> UnionProps.tryLast (function SelectorProp.SourceList x -> Some x | _ -> None) |> Option.defaultValue []
      let displayer     = props |> UnionProps.tryLast (function SelectorProp.Displayer x -> Some x | _ -> None) |> Option.defaultValue (fun (_, v) -> v.ToString() |> str)
      let inputAttrs    = props |> UnionProps.concat (function SelectorProp.InputAttrs x -> Some x | _ -> None)
      let outerClasses  = props |> UnionProps.concat (function SelectorProp.OuterClasses x -> Some x | _ -> None) |> Seq.toList
      let labelClasses  = props |> UnionProps.concat (function SelectorProp.LabelClasses x -> Some x | _ -> None) |> Seq.toList
      let inputClasses  = props |> UnionProps.concat (function SelectorProp.InputClasses x -> Some x | _ -> None) |> Seq.toList
      let errorClasses  = props |> UnionProps.concat (function SelectorProp.ErrorClasses x -> Some x | _ -> None) |> Seq.toList

      let ids =
        try field |> getFormFieldValue :?> 'id seq
        with _ -> [||] :> 'id seq

      let generateValue id =
        ids
        |> List.ofSeq
        |> List.partition (fun id' -> id = id')
        |> function
            | [], x -> id::x
            | _, x  -> x
        |> List.toArray

      fieldOuter outerClasses
        [
          if label.IsSome then yield fieldLabel labelClasses label.Value

          for (id, v) in sourceList do
            yield div
              [
                Classes inputClasses
              ]
              [
                yield input [
                  yield! inputAttrs
                  yield Type "checkbox"
                  yield Value (ids |> Seq.exists ((=) id))
                  yield OnChange (fun _ -> ChangeField (field.Name, box (generateValue id)) |> disp)
                ]

                yield displayer (id, v)
              ]

          yield! fieldError errorClasses field.Value
        ]
