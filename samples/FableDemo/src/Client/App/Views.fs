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
      button [
        OnClick onClick
        Classes [
          yield tailwind.``m-02``
          yield tailwind.``py-01``
          yield tailwind.``px-02``
          yield tailwind.rounded
          yield! classes
        ]
      ] [
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


type MyLightForm() =
  static member formOuterClasses =
    classes [
      tailwind.border
      tailwind.``border-gray-200``
      tailwind.``border-2``
      tailwind.``p-02``
      tailwind.``m-02``
      tailwind.rounded
    ]

  static member formLabelClasses =
    classes [
      tailwind.``text-gray-600``
      tailwind.``text-sm``
      tailwind.``pb-01``
    ]

  static member formErrorClasses =
    classes [
      tailwind.``text-red-400``
      tailwind.``mt-01``
      tailwind.``text-sm``
    ]

  static member input(label, ty, ?leftIconClasses, ?rightIconClasses, ?inputClasses) =
    let iconView iconClasses =
      icon
        [
          yield tailwind.block
          yield tailwind.``bg-gray-300``
          yield tailwind.``py-02``
          yield tailwind.``px-02``
          yield tailwind.``text-center``
          yield! iconClasses
        ]

    Fields.input
      (ty
      ,label = label
      ,outerClasses = MyLightForm.formOuterClasses
      ,inputClasses =
          classes [
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
            if leftIconClasses.IsNone then
              yield tailwind.``rounded-l``
            if rightIconClasses.IsNone then
              yield tailwind.``rounded-r``
            if inputClasses.IsSome then
              yield! inputClasses.Value
          ]
      ,labelClasses = MyLightForm.formLabelClasses
      ,errorClasses = MyLightForm.formErrorClasses
      ,inputViewWrapper = fun v ->
          div
            [
              Classes
                [
                  tailwind.flex
                ]
            ]
            [
              if leftIconClasses.IsSome then
                yield iconView [ yield tailwind.``rounded-l``; yield! leftIconClasses.Value ]

              yield v

              if rightIconClasses.IsSome then
                yield iconView [ yield tailwind.``rounded-r``; yield! rightIconClasses.Value ]
            ])


  static member selects(label, sourceList, displayer) =
    Fields.selector
      (sourceList
      ,label = label
      ,displayer = displayer
      ,outerClasses = MyLightForm.formOuterClasses
      ,inputClasses =
          classes [
            tailwind.flex
            tailwind.``items-center``
            tailwind.``text-gray-700``
            tailwind.``ml-01``
          ]
      ,labelClasses = MyLightForm.formLabelClasses
      ,errorClasses = MyLightForm.formErrorClasses)


  static member errorSummary form =
    div [
      Classes [
        tailwind.``text-red-500``
        tailwind.``text-sm``
        tailwind.``text-center``
        tailwind.``p-02``
      ]
    ] [
      for e in getFormErrors form -> div [] [ sprintf "* %s" e |> str ]
    ]


let app state dispatch =
    let field = Fields.field state.UserProfileForm (UserProfileMsg >> dispatch)

    div
      [
        Classes
          [
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
            Classes
              [
                tailwind.``sm:w-full``
                tailwind.``md:w-01/04``
                tailwind.``shadow-lg``
              ]
          ]
          [
            field "UserName"
              (MyLightForm.input
                ("Email"
                ,InputType.Email
                ,leftIconClasses = [ fontAwsome.fa; fontAwsome.``fa-mail-bulk`` ]))

            field "Password"
              (MyLightForm.input
                ("Password"
                ,InputType.Password
                ,leftIconClasses = [ fontAwsome.fa; fontAwsome.``fa-lock``; tailwind.``text-green-500`` ]
                ,inputClasses = [ tailwind.``text-green-400`` ]))

            field "Birthday"
              (MyLightForm.input
                ("Birthday"
                ,InputType.Date "yyyy/MM/dd"
                ,inputClasses = [ tailwind.``text-purple-500`` ]))

            field "Roles"
              (MyLightForm.selects
                ("Roles"
                ,[ 1, "R1"; 2, "R2" ]
                ,fun (_,v) -> span [ Classes [ tailwind.``ml-01`` ] ] [ str v ]))

            field "Address:Country"
              (MyLightForm.input
                ("Country"
                ,InputType.Text
                ,rightIconClasses = [ fontAwsome.fa; fontAwsome.``fa-map-marked`` ]))

            field "Address:Street"
              (MyLightForm.input
                ("Street"
                ,InputType.Text
                ,leftIconClasses = [ fontAwsome.fa; fontAwsome.``fa-funnel-dollar``; tailwind.``text-orange-400`` ]
                ,rightIconClasses = [ fontAwsome.fa; fontAwsome.``fa-passport``; tailwind.``text-red-500`` ]))

            field "Address:ZipCode"
              (MyLightForm.input
                ("ZipCode"
                ,InputType.Number))

            MyLightForm.errorSummary state.UserProfileForm

            div
              [
                Classes
                  [
                    tailwind.``text-center``
                    tailwind.``mb-02``
                  ]
              ]
              [
                Buttons.primaryButton  "Submit" ignore
                Buttons.secondayButton "Cancel" ignore
              ]
            icon
              [
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
            Classes
              [
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
            Classes
              [
                tailwind.``text-gray-500``
                tailwind.``text-center``
                tailwind.``w-full``
              ]
          ]
          [
            str (sprintf "%A" UserProfile.defaultValue)
          ]
      ]
