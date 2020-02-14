[<AutoOpen>]
module Fun.LightForm.FormView.Field

open System
open Fable.React
open Fable.React.Props


type ISimpleFieldProp = interface end

[<RequireQualifiedAccess>]
type SimpleFieldProp =
  | Label of string
  | Errors of string list 
  | OuterClasses of string list
  | LabelClasses of string list
  | ErrorClasses of string list
  | FieldView of ReactElement
  | OuterAttrs of IHTMLProp list
  interface ISimpleFieldProp


[<RequireQualifiedAccess>]
type InputProp<'T> =
  | Label of string
  | Value of 'T
  | AlwaysRerender of bool
  | InputValue of InputValue
  | DisplayValueConverter of (InputValue -> obj)
  | ConvertTo of ('T -> InputValue)
  | ConvertFrom of (obj -> 'T)
  | OnValueChange of ('T -> unit)
  | InputViewWrapper of (ReactElement -> ReactElement)
  | LeftView of ReactElement
  | RightView of ReactElement
  | ContainerAttrs of IHTMLProp list
  | InputAttrs of IHTMLProp list
  | SimpleFieldProps of ISimpleFieldProp list
  interface ISimpleFieldProp
and [<RequireQualifiedAccess>] InputValue =
  | Text of string
  | Email of string
  | Number of float
  | Password of string
  | Date of DateTime


[<RequireQualifiedAccess>]
type TextAreaProp =
  | Label of string
  | Value of string
  | OnValueChange of (string -> unit)
  | Attrs of IHTMLProp list
  | SimpleFieldProps of ISimpleFieldProp list


[<RequireQualifiedAccess>]
type DropdownProp =
  | HeaderAttrs of IHTMLProp list
  | DropdownAttrs of IHTMLProp list
  | Position of DropdownPosition
and [<RequireQualifiedAccess>] DropdownPosition =
  | Left
  | Right


[<RequireQualifiedAccess>]
type SelectorProp<'Id, 'Value> =
  | Label of string
  | Placeholder of string
  | Source of ('Id * 'Value) list
  | Displayer of ('Id * 'Value -> ReactElement)
  | SelectedIds of 'Id list
  | OnSelect of ('Id list -> unit)
  | SelectionsContainerAttrs of IHTMLProp list
  | InputAttrs of IHTMLProp list
  | SelectionClasses of string list
  | SwitchType of SwitchType
  | OnlyOne of bool
  | AtLeastOne of bool
  | EnableDropdown of bool
  | DropdownProps of DropdownProp list
  | SimpleFieldProps of ISimpleFieldProp list
and [<RequireQualifiedAccess>] SwitchType =
  | CheckBox
  | Radio
  | Flat


let partitionProps (props: IHTMLProp list) =
  props
  |> List.partition (function
    | :? HTMLPropExtra -> true
    | _ -> false)
  |> fun (extras, attrs) ->
      extras |> List.map (fun x -> x :?> HTMLPropExtra)
      , attrs


let fieldLabel cs label =
    div </> [
      match cs with
        | [] -> Style [ Color "gray"; FontSize "0.9rem" ]
        | cs -> Classes cs
      Text label
    ]

let fieldError cs es =
    [
      for e in es ->
        div </> [
          match cs with
            | [] -> Style [ Color "red"; FontSize "0.8rem"; Opacity "0.8" ]
            | cs -> Classes cs
          Text e
        ]
    ]


let simpleField (props: ISimpleFieldProp list) =
    let props = props |> List.choose (function :? SimpleFieldProp as x -> Some x | _ -> None)
    let label             = props |> UnionProps.tryLast (function SimpleFieldProp.Label x -> Some x | _ -> None)
    let errorClasses      = props |> UnionProps.tryLast (function SimpleFieldProp.ErrorClasses x -> Some x | _ -> None) |> Option.defaultValue []
    let outerClasses      = props |> UnionProps.concat (function SimpleFieldProp.OuterClasses x -> Some x | _ -> None)
    let labelClasses      = props |> UnionProps.concat (function SimpleFieldProp.LabelClasses x -> Some x | _ -> None)

    div </> [
      yield! props |> UnionProps.concat (function SimpleFieldProp.OuterAttrs x -> Some x | _ -> None)
      match outerClasses with
        | [] -> Style [ Margin "5px"; Padding "5px" ]
        | cs -> Classes cs
      Children [
        if label.IsSome then fieldLabel labelClasses label.Value 
    
        props
        |> UnionProps.tryLast (function SimpleFieldProp.FieldView x -> Some x | _ -> None)
        |> Option.defaultValue emptyView

        yield! 
          props 
          |> UnionProps.concat (function SimpleFieldProp.Errors x -> Some x | _ -> None)
          |> fieldError errorClasses
      ]
    ]


let inputField (props: InputProp<_> list) =
    let onValueChange     = props |> UnionProps.tryLast (function InputProp.OnValueChange x -> Some x | _ -> None)
    let inputViewWrapper  = props |> UnionProps.tryLast (function InputProp.InputViewWrapper x -> Some x | _ -> None)
    let leftView          = props |> UnionProps.tryLast (function InputProp.LeftView x -> Some x | _ -> None)
    let rightView         = props |> UnionProps.tryLast (function InputProp.RightView x -> Some x | _ -> None)
    let alwaysRerender    = props |> UnionProps.tryLast (function InputProp.AlwaysRerender x -> Some x | _ -> None) |> Option.defaultValue false

    let inputExtraProps, inputAttrs = props |> UnionProps.concat (function InputProp.InputAttrs x -> Some x | _ -> None) |> partitionProps

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
            | Some v, _ when box v |> isNull |> not -> v |> box |> string |> InputValue.Text
            | Some _, _
            | None, _                -> InputValue.Text ""

    let inputView = 
      input [
        if alwaysRerender then
          Key (Random().Next(0, 10000000).ToString())
        match inputExtraProps |> UnionProps.concat (function HTMLPropExtra.Classes x -> Some x | _ -> None) with
          | [] -> Style [ Width "100%"; Margin "2px 0"; Padding "2px 5px"; BackgroundColor "#f1f1f1" ]
          | cs -> classes cs
        Type (
          match value with
            | InputValue.Text _      -> "text"
            | InputValue.Email _     -> "email"
            | InputValue.Date _      -> "date"
            | InputValue.Number _    -> "number"
            | InputValue.Password _  -> "password")
        DefaultValue (
          props
          |> UnionProps.tryLast (function InputProp.DisplayValueConverter x -> Some x | _ -> None)
          |> function
            | Some converter -> converter value
            | None ->
                match value with
                  | InputValue.Text x      -> box x
                  | InputValue.Email x     -> box x
                  | InputValue.Password x  -> box x
                  | InputValue.Number x    -> box x
                  | InputValue.Date x      -> if x |> box |> isNull then box "" else box (x.ToString("yyyy-MM-dd")))
        OnChange (fun e ->
          match onValueChange with
            | Some dispatch ->
                props 
                |> UnionProps.tryLast (function InputProp.ConvertFrom x -> Some x | _ -> None)
                |> function
                  | Some cf -> cf e.Value |> dispatch
                  | None    ->
                      match value with
                      | InputValue.Text _ 
                      | InputValue.Email _
                      | InputValue.Password _  -> e.Value |> unbox |> dispatch
                      | InputValue.Date _      -> DateTime.Parse e.Value |> unbox |> dispatch
                      | InputValue.Number _    -> Convert.ToDouble e.Value |> unbox |> dispatch
            | None ->
                ())
        yield! inputAttrs
      ]

    simpleField [
      yield! props |> UnionProps.concat (function InputProp.SimpleFieldProps x -> Some x | _ -> None)
      match props |> UnionProps.tryLast (function InputProp.Label x -> Some x | _ -> None) with
        | Some x -> SimpleFieldProp.Label x
        | None   -> ()
      SimpleFieldProp.FieldView (div </> [
        match props |> UnionProps.concat (function InputProp.ContainerAttrs x -> Some x | _ -> None) with
          | [] -> Style [ Display DisplayOptions.Flex ]
          | x  -> yield! x
        Children [
          match leftView with
            | Some v -> v
            | None   -> ()
          match inputViewWrapper with
            | Some wrapper -> wrapper inputView
            | None         -> inputView
          match rightView with
            | Some v -> v
            | None   -> ()
        ]
      ])
    ]


let textAreaField (props: TextAreaProp list) =
    let value             = props |> UnionProps.tryLast (function TextAreaProp.Value x -> Some x | _ -> None) |> Option.defaultValue ""
    let onValueChange     = props |> UnionProps.tryLast (function TextAreaProp.OnValueChange x -> Some x | _ -> None)
    let extraProps, attrs = props |> UnionProps.concat (function TextAreaProp.Attrs x -> Some x | _ -> None) |> partitionProps

    let fieldView = 
      textarea </> [
        extraProps
        |> UnionProps.concat (function HTMLPropExtra.Classes x -> Some x | _ -> None)
        |> function
          | [] -> Style [ Width "100%"; Margin "2px 0"; Padding "2px 5px"; BackgroundColor "#f1f1f1" ] |> unbox<IHTMLProp>
          | cs -> Classes cs |> unbox<IHTMLProp>
        DefaultValue value
        OnChange (fun e ->
          match onValueChange with
            | Some dispatch -> e.Value |> dispatch
            | None -> ())
        yield! attrs
      ]

    simpleField [
      yield! props |> UnionProps.concat (function TextAreaProp.SimpleFieldProps x -> Some x | _ -> None)
      match props |> UnionProps.tryLast (function TextAreaProp.Label x -> Some x | _ -> None) with
        | Some x -> SimpleFieldProp.Label x
        | None   -> ()
      SimpleFieldProp.FieldView fieldView
    ]


let dropdown =
  FunctionComponent.Of(
    (fun (props: DropdownProp list) ->
      let position = props |> UnionProps.tryLast (function DropdownProp.Position x -> Some x | _ -> None) |> Option.defaultValue DropdownPosition.Right
      let headerAttrs = props |> UnionProps.concat (function DropdownProp.HeaderAttrs x -> Some x | _ -> None)
      let dropdownAttrs = props |> UnionProps.concat (function DropdownProp.DropdownAttrs x -> Some x | _ -> None)

      let showDropdown = Hooks.useState false
      let toggel _ = showDropdown.update (not showDropdown.current)

      div </> [
        Style [ Position PositionOptions.Relative ]
        OnMouseEnter toggel
        OnMouseLeave toggel
        Children [
          div </> [
            yield! headerAttrs
            OnClick toggel
          ]
          if showDropdown.current then
            div </> [
              yield! dropdownAttrs
              Style [
                Position PositionOptions.Absolute
                match position with
                  | DropdownPosition.Right -> Right 0
                  | DropdownPosition.Left  -> Left 0
              ]
            ]
        ]
      ])
     , "dropdown"
  )


let inline selectorField (props: SelectorProp<_, _> list) =
    let defaultDisplayer x =
      span </> [
        Style [ MarginLeft "10px" ]
        Text (x |> snd |> box |> string)
      ]

    let switchType    = props |> UnionProps.tryLast (function SelectorProp.SwitchType x -> Some x | _ -> None) |> Option.defaultValue SwitchType.CheckBox
    let ids           = props |> UnionProps.tryLast (function SelectorProp.SelectedIds x -> Some x | _ -> None) |> Option.defaultValue []
    let sourceList    = props |> UnionProps.tryLast (function SelectorProp.Source x -> Some x | _ -> None) |> Option.defaultValue []
    let displayer     = props |> UnionProps.tryLast (function SelectorProp.Displayer x -> Some x | _ -> None) |> Option.defaultValue defaultDisplayer
    let onlyOne       = props |> UnionProps.tryLast (function SelectorProp.OnlyOne x -> Some x | _ -> None) |> Option.defaultValue false
    let atLeastOne    = props |> UnionProps.tryLast (function SelectorProp.AtLeastOne x -> Some x | _ -> None) |> Option.defaultValue false
    let enableDrop    = props |> UnionProps.tryLast (function SelectorProp.EnableDropdown x -> Some x | _ -> None) |> Option.defaultValue false
    let dropdownProps = props |> UnionProps.concat  (function SelectorProp.DropdownProps x -> Some x | _ -> None)
    let inputAttrs    = props |> UnionProps.concat  (function SelectorProp.InputAttrs x -> Some x | _ -> None)
    let dispatch      = props |> UnionProps.tryLast (function SelectorProp.OnSelect x -> Some x | _ -> None)

    let ids =
      match onlyOne, ids with
        | true, x::_ -> [x]
        | _ -> ids

    match ids, sourceList, atLeastOne, dispatch with
      | [], (id, _)::_, true, Some dispatch -> dispatch [id]
      | _ -> ()

    let generateValues id =
      ids
      |> List.partition (fun id' -> id = id')
      |> function
          | [], x -> if onlyOne then [id] else id::x
          | _, x  -> if onlyOne then [] else x

    let dispatchValueChange id =
      match ids, atLeastOne, dispatch with
      | [x], true, _ when x = id -> ()
      | _, _, Some dispatch -> generateValues id |> dispatch
      | _ -> ()

    let switchView id =
      input [
        Type (
          match switchType with
            | SwitchType.CheckBox -> "checkbox"
            | SwitchType.Radio    -> "radio"
            | SwitchType.Flat     -> "text")
        Checked (ids |> Seq.exists ((=) id))
        OnChange (fun _ -> dispatchValueChange id)
        yield! inputAttrs
      ]

    let switcher id =
      match switchType with
        | SwitchType.CheckBox
        | SwitchType.Radio -> switchView id
        | SwitchType.Flat -> span </> []

    let fieldView =
      div </> [
        yield! props |> UnionProps.concat (function SelectorProp.SelectionsContainerAttrs x -> Some x | _ -> None)
        Children [
          for (id, v) in sourceList do
            div </> [
              Classes (props |> UnionProps.concat (function SelectorProp.SelectionClasses x -> Some x | _ -> None))
              Children [
                switcher id
                displayer (id, v)
              ]
            ]
        ]
      ]

    simpleField [
      yield! props |> UnionProps.concat (function SelectorProp.SimpleFieldProps x -> Some x | _ -> None)
      match props |> UnionProps.tryLast (function SelectorProp.Label x -> Some x | _ -> None) with
        | Some x -> SimpleFieldProp.Label x
        | None   -> ()
      SimpleFieldProp.FieldView (
        if enableDrop then
          dropdown [
            DropdownProp.HeaderAttrs [
              Children [
                span </> [
                  Text (
                    sourceList
                    |> List.filter (fun (id, _) -> ids |> List.contains id)
                    |> List.map (snd >> box >> string)
                    |> function
                      | [] -> props |> UnionProps.tryLast (function SelectorProp.Placeholder x -> Some x | _ -> None) |> Option.defaultValue "---"
                      | x  -> x |> String.concat ", "
                  )
                ]
              ]
            ]
            DropdownProp.DropdownAttrs [
              Children [ fieldView ]
            ]
            DropdownProp.Position DropdownPosition.Left
            yield! dropdownProps
          ]
        else fieldView
      )
    ]
