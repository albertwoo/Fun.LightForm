module Client.App.Views

open System
open Fable.React
open Fable.React.Props
open Zanaptak.TypedCssClasses
open Fun.LightForm.Views

type tailwind = CssClasses<"./public/css/tailwind-generated.css", Naming.Verbatim>
type fontAwsome = CssClasses<"./public/css/font-awesome-v5-10-2.min.css", Naming.Verbatim>


let classes str = str |> List.filter (String.IsNullOrEmpty >> not) |> String.concat " "
let Classes = classes >> Class

let emptyView = div [ Style [ Display DisplayOptions.None ] ] []

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

let formInput ty label =
  formInput
    { OuterClasses = formOuterClasses
      LabelClasses = formLabelClasses
      ErrorClasses = formErrorClasses
      InputClasses =
        classes [
          tailwind.``outline-none``
          tailwind.``bg-gray-200``
          tailwind.``py-01``
          tailwind.``px-03``
          tailwind.``w-full``
          tailwind.``focus:border-blue-400``
          tailwind.``focus:bg-blue-100``
          tailwind.``text-gray-700``
        ]
      Label = label
      Type = ty }

let formSelects label sourceList displayer =
  formSelects
    { OuterClasses = formOuterClasses
      LabelClasses = formLabelClasses
      ErrorClasses = formErrorClasses
      InputClasses =
        classes [
          tailwind.flex
          tailwind.``items-center``
          tailwind.``text-gray-700``
          tailwind.``ml-01``
        ]
      Label = label
      SourceList = sourceList
      Displayer = displayer }


let app state dispatch =
    let formEl = formElement state.UserProfileForm (UserProfileMsg >> dispatch)

    div [
      Classes [
        tailwind.``font-sans``
        tailwind.``h-screen``
        tailwind.flex
        tailwind.``flex-wrap``
        tailwind.``justify-center``
        tailwind.``items-center``
      ]
    ] [
      form [
        Classes [
          tailwind.``sm:w-full``
          tailwind.``md:w-01/04``
          tailwind.``shadow-lg``
        ]
      ] [
        formEl "UserName"         (formInput InputType.Email "Email")
        formEl "Password"         (formInput InputType.Password "Password")
        formEl "Birthday"         (formInput (InputType.DateTime "yyyy/MM/dd") "Birthday")
        formEl "Roles"            (formSelects "Roles" [ 1, "R1"; 2, "R2" ] (fun (_,v) -> span [ Classes [ tailwind.``ml-01`` ] ] [ str v ]))
        formEl "Address:Country"  (formInput InputType.Text "Country")

        button [
          Classes [
            tailwind.``m-02``
            tailwind.``py-01``
            tailwind.``px-02``
            tailwind.rounded
            tailwind.``text-white``
            tailwind.``bg-green-400``
            tailwind.``hover:bg-green-500``
          ]
        ] [
          str "Submit"
        ]
      ]

      div [
        Classes [
          tailwind.``text-gray-600``
        ]
      ] [
        str (sprintf "%A" state.UserProfileForm)
      ]
    ]
