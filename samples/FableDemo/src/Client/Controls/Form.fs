module Client.Controls.Form

open Fable.React
open Fable.React.Props
open Fun.LightForm
open Fun.LightForm.FormView


let formOuterClasses =
  [
    Tw.border
    Tw.``border-gray-200``
    Tw.``border-2``
    Tw.``p-02``
    Tw.``m-02``
    Tw.rounded
  ]

let formLabelClasses =
  [
    Tw.``text-gray-600``
    Tw.``text-sm``
    Tw.``pb-01``
  ]

let formErrorClasses =
  [
    Tw.``text-red-400``
    Tw.``mt-01``
    Tw.``text-sm``
  ]


let input props =
  let leftView  = props |> UnionProps.tryLast (function InputProp.LeftView x -> Some x | _ -> None)
  let rightView = props |> UnionProps.tryLast (function InputProp.RightView x -> Some x | _ -> None)

  let iconView props =
    div </> [
      Classes [
        Tw.``bg-gray-300``
        Tw.``py-01``
        Tw.``px-01``
        Tw.``text-center``
      ]
      yield! props
    ]

  Form.input [
    InputProp.InputAttrs [
      Classes [
        Tw.``outline-none``
        Tw.``bg-gray-200``
        Tw.``py-01``
        Tw.``px-03``
        Tw.``w-full``
        Tw.``focus:border-blue-400``
        Tw.``focus:bg-blue-100``
        Tw.``hover:bg-blue-200``
        Tw.``text-gray-700``
        Tw.``rounded-none``
        if leftView.IsNone then Tw.``rounded-l``
        if rightView.IsNone then Tw.``rounded-r``
      ]
    ]
    InputProp.SimpleFieldProps [
      SimpleFieldProp.OuterClasses formOuterClasses
      SimpleFieldProp.LabelClasses formLabelClasses
      SimpleFieldProp.ErrorClasses formErrorClasses
    ]
    match leftView with
      | None   -> ()
      | Some v ->
          InputProp.LeftView (iconView [
            Classes [ Tw.``rounded-l`` ]
            Children [ v ]
          ])
    match rightView with
      | None   -> ()
      | Some v ->
          InputProp.RightView (iconView [
            Classes [ Tw.``rounded-r`` ]
            Children [ v ]
          ])
    yield!
      props
      |> List.filter (function
          | InputProp.LeftView _
          | InputProp.RightView _ -> false
          | _ -> true)
  ]


let textArea props =
  Form.textArea [
    TextAreaProp.Attrs [
      Classes [
        Tw.``py-01``
        Tw.``px-02``
        Tw.``w-full``
        Tw.``bg-gray-200``
        Tw.``focus:bg-gray-100``
        Tw.border
        Tw.``border-gray-200``
        Tw.``focus:border-gray-300``
        Tw.``outline-none``
        Tw.rounded
      ]
    ]
    TextAreaProp.SimpleFieldProps [
      SimpleFieldProp.OuterClasses formOuterClasses
      SimpleFieldProp.LabelClasses formLabelClasses
      SimpleFieldProp.ErrorClasses formErrorClasses
    ]
    yield! props
  ]


let inline selector props =
  let dropDownProps = props |> UnionProps.concat (function SelectorProp.DropdownProps x -> Some x | _ -> None)
  Form.selector [
    SelectorProp.SelectionClasses [
      Tw.flex
      Tw.``items-center``
      Tw.``text-gray-700``
      Tw.``w-full``
      if dropDownProps |> Seq.isEmpty |> not then
        Tw.rounded
        Tw.``bg-gray-200``
        Tw.``py-01``
        Tw.``px-01``
        Tw.border
        Tw.``border-gray-200``
        Tw.``focus:bg-gray-100``
        Tw.``focus:border-gray-300``
        Tw.``outline-none``
    ]
    SelectorProp.SimpleFieldProps [
      SimpleFieldProp.OuterClasses formOuterClasses
      SimpleFieldProp.LabelClasses formLabelClasses
      SimpleFieldProp.ErrorClasses formErrorClasses
    ]
    yield! props
  ]


let errorSummary form =
  div </> [
    Classes [
      Tw.``text-red-500``
      Tw.``text-sm``
      Tw.``text-center``
      Tw.``p-02``
    ]
    Children [
      for e in getFormErrors form -> div </> [ HTMLPropExtra.Text (sprintf "* %s" e) ]
    ]
  ]
