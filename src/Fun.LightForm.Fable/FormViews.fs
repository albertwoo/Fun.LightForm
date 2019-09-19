module Fun.LightForm.FormViews

open System
open Fable.React
open Fable.React.Props


type FieldRenderer = FieldRenderer<Fable.React.ReactElement>


let field (state: LightForm) dispatch key (render: FieldRenderer) =
  state
  |> List.tryFind (fun x -> x.Name = key)
  |> Option.map (fun field ->
      fragment 
          [ FragmentProp.Key field.Name ] 
          [ render field dispatch ])
  |> Option.defaultValue 
      (div [ Style [ Color "red" ] ] 
           [ str (sprintf "Field is not configured for %s" key) ])


let fieldOuter children classes =
  div
    [
      match classes with
      | Some cs -> yield Class cs
      | None    -> yield Style [ Margin "5px"; Padding "5px" ]
    ]
    children


let fieldLabel label classes =
  div
    [
      match classes with
      | Some cs -> yield Class cs
      | None    -> yield Style [ Color "gray"; FontSize "0.9rem" ]
    ]
    [ str label ]


let fieldError fieldValue classes =
    match fieldValue with
    | Valid _ -> []
    | Invalid (_, es) ->
         [
            for e in es ->
              div
                [
                  match classes with
                  | Some cs -> yield Class cs
                  | None    -> yield Style [ Color "red"; FontSize "0.8rem"; Opacity "0.8" ]
                ]
                [ str e ]
         ]

let reduceClasses cs = cs |> String.concat " "
let inline private foldClasses defaultValue props = props |> UnionCase.caseValues defaultValue |> UnionCase.toOptions |> Option.map reduceClasses

type InputProp =
  | InputType of InputType
  | Label of string
  | OuterClasses of string
  | InputClasses of string
  | LabelClasses of string
  | ErrorClasses of string
  | InputViewWrapper of (ReactElement -> ReactElement)
and InputType =
  | Email
  | Text
  | Number
  | Password
  | Date of format: string

let inputField (props: InputProp list): FieldRenderer =
    fun field dispatch ->

      let label             = props |> UnionCase.caseValues (InputProp.Label "") |> UnionCase.toOption
      let inputType         = props |> UnionCase.caseValues (InputProp.InputType InputType.Text) |> UnionCase.value
      let inputViewWrapper  = props |> UnionCase.caseValues (InputProp.InputViewWrapper id) |> UnionCase.toOption
      let outerClasses      = props |> foldClasses (InputProp.OuterClasses "")
      let labelClasses      = props |> foldClasses (InputProp.LabelClasses "")
      let inputClasses      = props |> foldClasses (InputProp.InputClasses "")
      let errorClasses      = props |> foldClasses (InputProp.ErrorClasses "")

      let inputView = 
        input
          [
            match inputClasses with
              | None    -> yield Style [ Width "100%"; Margin "2px 0"; Padding "2px 5px"; BackgroundColor "#f1f1f1" ]
              | Some cs -> yield Class cs
            yield Type (
                  match inputType with
                  | Email -> "email"
                  | Date _ -> "date"
                  | Text -> "text"
                  | Number -> "number"
                  | Password -> "password")
            yield DefaultValue (
                let value = field |> getFormFieldValue
                match inputType with
                | Email
                | Text
                | Password
                | Number -> if isNull value then box "" else box value
                | Date f ->
                    try (value :?> DateTime).ToString(f) |> box
                    with _ -> box value)
            yield OnChange (fun e ->
                match inputType with
                | Email
                | Text
                | Password
                | Number
                | Date _ -> ChangeField (field.Name, e.Value)
                |> dispatch)
          ]
      fieldOuter
        [
          if label.IsSome then
            yield fieldLabel label.Value labelClasses

          match inputViewWrapper with
          | Some wrapper -> yield wrapper inputView
          | None         -> yield inputView

          yield! fieldError field.Value errorClasses
        ]
       outerClasses



type SelectorProp<'id, 'v> =
  | SourceList of ('id * 'v) list
  | Displayer of ('id * 'v -> ReactElement)
  | Label of string
  | OuterClasses of string
  | LabelClasses of string
  | ErrorClasses of string
  | InputClasses of string

let inline selectorField<'id, 'v when 'id : equality> (props: SelectorProp<'id, 'v> list): FieldRenderer =
    fun field disp ->
      let ids =
        try field |> getFormFieldValue :?> 'id seq
        with _ -> [||] :> 'id seq

      let createNewValue id =
        ids
        |> List.ofSeq
        |> List.partition (fun id' -> id = id')
        |> function
          | [], x -> id::x
          | _, x  -> x
        |> List.toArray

      let label         = props |> UnionCase.caseValues (SelectorProp.Label "") |> UnionCase.toOption
      let sourceList    = props |> UnionCase.caseValues (SelectorProp.SourceList []) |> UnionCase.value
      let displayer     = props |> UnionCase.caseValues (SelectorProp.Displayer (fun (_, v) -> v.ToString() |> str)) |> UnionCase.value
      let outerClasses  = props |> foldClasses (SelectorProp.OuterClasses "")
      let labelClasses  = props |> foldClasses (SelectorProp.LabelClasses "")
      let inputClasses  = props |> foldClasses (SelectorProp.InputClasses "")
      let errorClasses  = props |> foldClasses (SelectorProp.ErrorClasses "")

      fieldOuter
        [
          if label.IsSome then
            yield fieldLabel label.Value labelClasses

          for (id, v) in sourceList do
            yield div
              [
                Class (inputClasses |> Option.defaultValue "")
              ]
              [
                yield input
                  [
                    Type "checkbox"
                    Value (ids |> Seq.exists ((=) id))
                    OnChange (fun _ -> ChangeField (field.Name, box (createNewValue id)) |> disp)
                  ]

                yield displayer (id, v)
              ]

          yield! fieldError field.Value errorClasses
        ]
        outerClasses
