module Client.App.Views

open System
open Fable.React
open Fable.React.Props
open Zanaptak.TypedCssClasses
open Fun.LightForm
open Fun.LightForm.FormViews

type tailwind = CssClasses<"./public/css/tailwind-generated.css", Naming.Verbatim>
type fontAwsome = CssClasses<"./public/css/font-awesome-v5-10-2.min.css", Naming.Verbatim>


let classes str = str |> List.filter (String.IsNullOrEmpty >> not) |> String.concat " "
let Classes = classes >> Class

let emptyView = div [ Style [ Display DisplayOptions.None ] ] []

let icon cs = span [ Classes cs ] []


module Buttons =
  let button classes label onClick =
      button
        [
          OnClick onClick
          Classes [
            yield tailwind.``m-02``
            yield tailwind.``py-01``
            yield tailwind.``px-02``
            yield tailwind.rounded
            yield! classes
          ]
        ]
        [
          str label
        ]

  let primaryButton =
      button [
        tailwind.``text-white``
        tailwind.``bg-green-400``
        tailwind.``hover:bg-green-500``
      ]

  let secondayButton =
      button [
        tailwind.``text-gray-700``
        tailwind.border
        tailwind.``hover:bg-gray-200``
      ]


module MyLightForm =
  let formOuterClasses =
    classes [
      tailwind.border
      tailwind.``border-gray-200``
      tailwind.``border-2``
      tailwind.``p-02``
      tailwind.``m-02``
      tailwind.rounded
    ]

  let formLabelClasses =
    classes [
      tailwind.``text-gray-600``
      tailwind.``text-sm``
      tailwind.``pb-01``
    ]

  let formErrorClasses =
    classes [
      tailwind.``text-red-400``
      tailwind.``mt-01``
      tailwind.``text-sm``
    ]

  type MyInputProp =
    | InputProps of InputProp list
    | LeftIconClasses of string list
    | RightIconClasses of string list

  let input props =
    let leftIconClasses  = props |> UnionCase.caseValues (MyInputProp.LeftIconClasses []) |> UnionCase.values |> List.concat
    let rightIconClasses = props |> UnionCase.caseValues (MyInputProp.RightIconClasses []) |> UnionCase.values |> List.concat

    let iconView iconClasses =
      icon [
        yield tailwind.block
        yield tailwind.``bg-gray-300``
        yield tailwind.``py-02``
        yield tailwind.``px-02``
        yield tailwind.``text-center``
        yield! iconClasses
      ]

    let inputWrapper v =
      div
        [
          Classes [ tailwind.flex ]
        ]
        [
          if List.isEmpty leftIconClasses |> not then
            yield iconView [ yield tailwind.``rounded-l``; yield! leftIconClasses ]

          yield v

          if List.isEmpty rightIconClasses |> not then
            yield iconView [ yield tailwind.``rounded-r``; yield! rightIconClasses ]
        ]

    inputField [
      yield InputProp.OuterClasses formOuterClasses
      yield InputProp.InputClasses (classes [
        yield tailwind.``outline-none``
        yield tailwind.``bg-gray-200``
        yield tailwind.``py-01``
        yield tailwind.``px-03``
        yield tailwind.``w-full``
        yield tailwind.``focus:border-blue-400``
        yield tailwind.``focus:bg-blue-100``
        yield tailwind.``hover:bg-blue-200``
        yield tailwind.``text-gray-700``
        yield tailwind.``rounded-none``
        if List.isEmpty leftIconClasses then
          yield tailwind.``rounded-l``
        if List.isEmpty rightIconClasses then
          yield tailwind.``rounded-r``
      ])
      yield InputProp.LabelClasses formLabelClasses
      yield InputProp.ErrorClasses formErrorClasses
      yield InputProp.InputViewWrapper inputWrapper
      yield! (props |> UnionCase.caseValues (MyInputProp.InputProps []) |> UnionCase.values |> List.concat)
    ]


  let inline selector props =
    selectorField [
      yield SelectorProp.OuterClasses formOuterClasses
      yield SelectorProp.InputClasses (classes [
        tailwind.flex
        tailwind.``items-center``
        tailwind.``text-gray-700``
        tailwind.``ml-01``
      ])
      yield SelectorProp.LabelClasses formLabelClasses
      yield SelectorProp.ErrorClasses formErrorClasses
      yield! props
    ]

  let errorSummary form =
    div
      [
        Classes [
          tailwind.``text-red-500``
          tailwind.``text-sm``
          tailwind.``text-center``
          tailwind.``p-02``
        ]
      ]
      [
        for e in getFormErrors form -> div [] [ sprintf "* %s" e |> str ]
      ]


open MyLightForm

let app state dispatch =
    let field key renderer arg = field state.UserProfileForm (UserProfileMsg >> dispatch) key (renderer arg)

    div
      [
        Classes [
          tailwind.``font-sans``
          tailwind.``h-screen``
          tailwind.flex
          tailwind.``flex-wrap``
          tailwind.``justify-center``
          tailwind.``items-center``
        ]
      ]
      [
        form
          [
            Classes [
              tailwind.``sm:w-full``
              tailwind.``md:w-01/04``
              tailwind.``shadow-lg``
            ]
          ]
          [
            field "UserName" input [
              MyInputProp.InputProps [
                InputProp.Label "Email"
                InputProp.InputType InputType.Email
              ]
              MyInputProp.LeftIconClasses [ fontAwsome.fa; fontAwsome.``fa-mail-bulk`` ]
            ]

            field "Password" input [
              MyInputProp.InputProps [
                InputProp.Label "Password"
                InputProp.InputType InputType.Password
              ]
              MyInputProp.LeftIconClasses [ fontAwsome.fa; fontAwsome.``fa-lock``; tailwind.``text-green-500`` ]
              MyInputProp.RightIconClasses [ tailwind.``text-green-400`` ]
            ]

            field "Birthday" input [
              MyInputProp.InputProps [
                InputProp.Label "Birthday"
                InputProp.InputType (InputType.Date "yyyy/MM/dd")
                InputProp.InputClasses (classes [ tailwind.``text-purple-500`` ])
              ]
            ]

            field "Roles" selector [
              SelectorProp.Label "Roles"
              SelectorProp.SourceList [ 1, "R1"; 2, "R2" ]
              SelectorProp.Displayer (fun (_,v) -> span [ Classes [ tailwind.``ml-01`` ] ] [ str v ])
            ]

            field "Address:Country" input [
              MyInputProp.InputProps [
                InputProp.Label "Country"
                InputProp.InputType InputType.Text
              ]
              MyInputProp.RightIconClasses [ fontAwsome.fa; fontAwsome.``fa-map-marked`` ]
            ]

            field "Address:Street" input [
              MyInputProp.InputProps [
                InputProp.Label "Street"
                InputProp.InputType InputType.Text
              ]
              MyInputProp.LeftIconClasses [ fontAwsome.fa; fontAwsome.``fa-funnel-dollar``; tailwind.``text-orange-400`` ]
              MyInputProp.RightIconClasses [ fontAwsome.fa; fontAwsome.``fa-passport``; tailwind.``text-red-500`` ]
            ]

            field "Address:ZipCode" input [
              MyInputProp.InputProps [
                InputProp.Label "ZipCode"
                InputProp.InputType InputType.Number
              ]
            ]

            errorSummary state.UserProfileForm

            div
              [
                Classes [
                  tailwind.``text-center``
                  tailwind.``mb-02``
                ]
              ]
              [
                Buttons.primaryButton  "Submit" ignore
                Buttons.secondayButton "Cancel" ignore
              ]
            icon [
              fontAwsome.fab
              fontAwsome.``fa-github``
              tailwind.block
              tailwind.``bg-blue-200``
              tailwind.``py-02``
              tailwind.``text-center``
              tailwind.``text-green-800``
            ]
          ]

        div
          [
            Classes [
              tailwind.``text-green-600``
              tailwind.``text-center``
              tailwind.``w-full``
            ]
          ]
          [
            str (sprintf "%A" (generateValueByForm typeof<UserProfile> UserProfile.defaultValue state.UserProfileForm))
          ]

        div
          [
            Classes [
              tailwind.``text-gray-500``
              tailwind.``text-center``
              tailwind.``w-full``
            ]
          ]
          [
            str (sprintf "%A" UserProfile.defaultValue)
          ]
      ]
