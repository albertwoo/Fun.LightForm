module Fun.LightForm.Field

open System
open Fable.React
open Fable.React.Props


let Classes = String.concat " " >> Class


let fieldOuter classes children =
  div [
    match classes with
      | [] -> yield Style [ Margin "5px"; Padding "5px" ]
      | cs -> yield Classes cs
  ] children

let fieldLabel classes label =
  div [
    match classes with
      | [] -> yield Style [ Color "gray"; FontSize "0.9rem" ]
      | cs -> yield Classes cs
  ] [ str label ]

let fieldError classes es =
  [
    for e in es ->
      div [
        match classes with
          | [] -> yield Style [ Color "red"; FontSize "0.8rem"; Opacity "0.8" ]
          | cs -> yield Classes cs
      ] [ str e ]
  ]


type SimpleFieldProp =
  | Label of string
  | Errors of string list 
  | OuterClasses of string list
  | LabelClasses of string list
  | ErrorClasses of string list
  | FieldView of ReactElement

let simpleField props =      
  let label             = props |> UnionProps.tryLast (function SimpleFieldProp.Label x -> Some x | _ -> None)
  let errorClasses      = props |> UnionProps.tryLast (function SimpleFieldProp.ErrorClasses x -> Some x | _ -> None) |> Option.defaultValue []
  let outerClasses      = props |> UnionProps.concat (function SimpleFieldProp.OuterClasses x -> Some x | _ -> None)
  let labelClasses      = props |> UnionProps.concat (function SimpleFieldProp.LabelClasses x -> Some x | _ -> None)

  fieldOuter outerClasses [
    if label.IsSome then yield fieldLabel labelClasses label.Value 

    yield
      props
      |> UnionProps.tryLast (function SimpleFieldProp.FieldView x -> Some x | _ -> None)
      |> Option.defaultValue (span [] [])

    yield! 
      props 
      |> UnionProps.concat (function SimpleFieldProp.Errors x -> Some x | _ -> None)
      |> fieldError errorClasses
  ]


type InputProp<'T> =
  | Label of string
  | Value of 'T
  | ConvertTo of ('T -> InputValue)
  | InputValue of InputValue
  | ConvertFrom of (obj -> 'T)
  | OnValueChange of ('T -> unit)
  | InputClasses of string list
  | InputViewWrapper of (ReactElement -> ReactElement)
  | InputAttrs of IHTMLProp list
  | SimpleFieldProps of SimpleFieldProp list
and InputValue =
  | Text of string
  | Email of string
  | Number of float
  | Password of string
  | Date of DateTime

let inputField (props: InputProp<_> list) =
      let onValueChange     = props |> UnionProps.tryLast (function InputProp.OnValueChange x -> Some x | _ -> None)
      let inputViewWrapper  = props |> UnionProps.tryLast (function InputProp.InputViewWrapper x -> Some x | _ -> None)
      let inputAttrs        = props |> UnionProps.concat (function InputProp.InputAttrs x -> Some x | _ -> None)
      let inputClasses      = props |> UnionProps.concat (function InputProp.InputClasses x -> Some x | _ -> None)

      let value =
        props
        |> UnionProps.tryLast (function InputProp.InputValue x -> Some x | _ -> None)
        |> function
          | Some x -> x
          | None ->
              let v = props |> UnionProps.tryLast (function InputProp.Value x -> Some x | _ -> None)
              let convertTo = props |> UnionProps.tryLast (function InputProp.ConvertTo x -> Some x | _ -> None)
              match v, convertTo with
              | Some v, Some converter -> converter v
              | Some v, _              -> v |> box |> string |> InputValue.Text
              | None, _                -> InputValue.Text ""

      let inputView = 
        input [
          yield! inputAttrs
          match inputClasses with
            | [] -> yield Style [ Width "100%"; Margin "2px 0"; Padding "2px 5px"; BackgroundColor "#f1f1f1" ]
            | cs -> yield Classes cs
          yield Type (
            match value with
              | Text _      -> "text"
              | Email _     -> "email"
              | Date _      -> "date"
              | Number _    -> "number"
              | Password _  -> "password")
          yield DefaultValue (
            match value with
              | Text x      -> box x
              | Email x     -> box x
              | Password x  -> box x
              | Number x    -> box x
              | Date x      -> box (x.ToString("yyyy-MM-dd")))
          yield OnChange (fun e ->
            match onValueChange with
              | Some dispatch ->
                  props 
                  |> UnionProps.tryLast (function InputProp.ConvertFrom x -> Some x | _ -> None)
                  |> function
                    | Some cf -> cf e.Value |> dispatch
                    | None    -> e.Value |> unbox |> dispatch
              | None ->
                  ())
        ]

      simpleField [
        yield! props |> UnionProps.concat (function InputProp.SimpleFieldProps x -> Some x | _ -> None)
        match props |> UnionProps.tryLast (function InputProp.Label x -> Some x | _ -> None) with
          | Some x -> yield SimpleFieldProp.Label x
          | None   -> ()
        yield SimpleFieldProp.FieldView (
          match inputViewWrapper with
            | Some wrapper -> wrapper inputView
            | None         -> inputView)
      ]


type SelectorProp<'Id, 'Value> =
  | Label of string
  | Source of ('Id * 'Value) list
  | Displayer of ('Id * 'Value -> ReactElement)
  | SelectedIds of 'Id list
  | OnIdsChange of ('Id list -> unit)
  | InputClasses of string list
  | InputAttrs of IHTMLProp list
  | SwitchType of SwitchType
  | OnlyOne of bool
  | SimpleFieldProps of SimpleFieldProp list
and SwitchType =
  | CheckBox
  | Radio

let inline selectorField (props: SelectorProp<_, _> list) =
      let defaultDisplayer x =
        span [
          Style [ MarginLeft "10px" ]
        ] [
          x |> snd |> box |> string |> str
        ]

      let switchType    = props |> UnionProps.tryLast (function SelectorProp.SwitchType x -> Some x | _ -> None) |> Option.defaultValue SwitchType.CheckBox
      let ids           = props |> UnionProps.tryLast (function SelectorProp.SelectedIds x -> Some x | _ -> None) |> Option.defaultValue []
      let sourceList    = props |> UnionProps.tryLast (function SelectorProp.Source x -> Some x | _ -> None) |> Option.defaultValue []
      let displayer     = props |> UnionProps.tryLast (function SelectorProp.Displayer x -> Some x | _ -> None) |> Option.defaultValue defaultDisplayer
      let onlyOne       = props |> UnionProps.tryLast (function SelectorProp.OnlyOne x -> Some x | _ -> None) |> Option.defaultValue false
      let inputAttrs    = props |> UnionProps.concat (function SelectorProp.InputAttrs x -> Some x | _ -> None)
      let inputClasses  = props |> UnionProps.concat (function SelectorProp.InputClasses x -> Some x | _ -> None) |> Seq.toList
      
      let ids =
        match onlyOne, ids with
        | true, x::_ -> [x]
        | _ -> ids

      let generateValues id =
        ids
        |> List.partition (fun id' -> id = id')
        |> function
            | [], x -> if onlyOne then [id] else id::x
            | _, x  -> if onlyOne then [] else x

      let fieldView =
        div [] [
          for (id, v) in sourceList do
            yield div [
                Classes inputClasses
              ] [
                yield input [
                  yield! inputAttrs
                  yield Type (
                    match switchType with
                      | SwitchType.CheckBox -> "checkbox"
                      | SwitchType.Radio    -> "radio")
                  yield Checked (ids |> Seq.exists ((=) id))
                  yield OnChange (fun _ -> 
                    props
                    |> UnionProps.tryLast (function SelectorProp.OnIdsChange x -> Some x | _ -> None)
                    |> function
                      | Some dispatch -> generateValues id |> dispatch
                      | None -> ())
                ]

                yield displayer (id, v)
              ]
        ]

      simpleField [
        yield! props |> UnionProps.concat (function SelectorProp.SimpleFieldProps x -> Some x | _ -> None)
        match props |> UnionProps.tryLast (function SelectorProp.Label x -> Some x | _ -> None) with
          | Some x -> yield SimpleFieldProp.Label x
          | None   -> ()
        yield SimpleFieldProp.FieldView fieldView
      ]
