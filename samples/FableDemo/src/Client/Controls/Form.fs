module Client.Controls.Form

open Fable.React
open Fable.React.Props
open Fun.LightForm
open Fun.LightForm.FormViews
open Fun.LightForm.Field


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
      yield Tw.block
      yield Tw.``bg-gray-300``
      yield Tw.``py-02``
      yield Tw.``px-02``
      yield Tw.``text-center``
      yield! iconClasses
    ]

  let inputWrapper v =
    div [
      Classes [ Tw.flex ]
    ] [
      if Seq.isEmpty leftIconClasses |> not then
        yield iconView [ yield Tw.``rounded-l``; yield! leftIconClasses ]

      yield v

      if Seq.isEmpty rightIconClasses |> not then
        yield iconView [ yield Tw.``rounded-r``; yield! rightIconClasses ]
    ]

  inputF [
    yield InputProp.InputClasses [
      yield Tw.``outline-none``
      yield Tw.``bg-gray-200``
      yield Tw.``py-01``
      yield Tw.``px-03``
      yield Tw.``w-full``
      yield Tw.``focus:border-blue-400``
      yield Tw.``focus:bg-blue-100``
      yield Tw.``hover:bg-blue-200``
      yield Tw.``text-gray-700``
      yield Tw.``rounded-none``
      if Seq.isEmpty leftIconClasses then yield Tw.``rounded-l``
      if Seq.isEmpty rightIconClasses then yield Tw.``rounded-r``
    ]
    yield InputProp.InputViewWrapper inputWrapper
    yield InputProp.SimpleFieldProps [
      SimpleFieldProp.OuterClasses formOuterClasses
      SimpleFieldProp.LabelClasses formLabelClasses
      SimpleFieldProp.ErrorClasses formErrorClasses
    ]
    yield! (props |> UnionProps.concat (function FormInputProp.InputProps x -> Some x | _ -> None))
  ]


let inline selector props =
  selectorF [
    yield SelectorProp.InputClasses [
      Tw.flex
      Tw.``items-center``
      Tw.``text-gray-700``
      Tw.``ml-01``
    ]
    yield SelectorProp.SimpleFieldProps [
      SimpleFieldProp.OuterClasses formOuterClasses
      SimpleFieldProp.LabelClasses formLabelClasses
      SimpleFieldProp.ErrorClasses formErrorClasses
    ]
    yield! props
  ]


let errorSummary form =
  div [
    Classes [
      Tw.``text-red-500``
      Tw.``text-sm``
      Tw.``text-center``
      Tw.``p-02``
    ]
  ] [
    for e in getFormErrors form -> div [] [ sprintf "* %s" e |> str ]
  ]
