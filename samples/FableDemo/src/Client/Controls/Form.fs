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


type FormInputProp<'T> =
  | InputProps of InputProp<'T> list
  | LeftIconClasses of string list
  | RightIconClasses of string list

let input props =
  let leftIconClasses  = props |> UnionProps.concat (function FormInputProp.LeftIconClasses x -> Some x | _ -> None)
  let rightIconClasses = props |> UnionProps.concat (function FormInputProp.RightIconClasses x -> Some x | _ -> None)

  let iconView iconClasses =
    Icon.icon [
      Classes [
        Tw.block
        Tw.``bg-gray-300``
        Tw.``py-02``
        Tw.``px-02``
        Tw.``text-center``
        yield! iconClasses
      ]
    ]

  let inputWrapper (v: ReactElement) =
    div </> [
      Classes [ Tw.flex ]
      Children [
        if Seq.isEmpty leftIconClasses |> not then
          iconView [ Tw.``rounded-l``; yield! leftIconClasses ]

        v

        if Seq.isEmpty rightIconClasses |> not then
          iconView [ Tw.``rounded-r``; yield! rightIconClasses ]
      ]
    ]

  Form.input [
    InputProp.InputClasses [
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
      if Seq.isEmpty leftIconClasses then Tw.``rounded-l``
      if Seq.isEmpty rightIconClasses then Tw.``rounded-r``
    ]
    InputProp.InputViewWrapper inputWrapper
    InputProp.SimpleFieldProps [
      SimpleFieldProp.OuterClasses formOuterClasses
      SimpleFieldProp.LabelClasses formLabelClasses
      SimpleFieldProp.ErrorClasses formErrorClasses
    ]
    yield! (props |> UnionProps.concat (function FormInputProp.InputProps x -> Some x | _ -> None))
  ]


let inline selector props =
  Form.selector [
    SelectorProp.InputClasses [
      Tw.flex
      Tw.``items-center``
      Tw.``text-gray-700``
      Tw.``ml-01``
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
