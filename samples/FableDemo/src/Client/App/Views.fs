module Client.App.Views

open System
open Fable.React
open Fable.React.Props
open Fun.LightForm
open Fun.LightForm.Field
open Fun.LightForm.FormViews
open Client.Controls
open Client.Controls.Form


let infoCard info classes =
  div [
    Classes [
      yield Tw.``text-gray-500``
      yield Tw.``text-center``
      yield Tw.``w-full``
      yield Tw.``py-02``
      yield Tw.``px-01``
      yield Tw.``m-02``
      yield Tw.``shadow-lg``
      yield! classes
    ]
  ] [
    str info
  ]


let app state dispatch =
    let field key renderer arg = field state.UserProfileForm (UserProfileMsg >> dispatch) key (renderer arg)

    div [
      Classes [
        Tw.``font-sans``
        Tw.``h-screen``
        Tw.flex
        Tw.``flex-col``
        Tw.``items-center``
      ]
    ] [
      form [
        Classes [
          Tw.``sm:w-full``
          Tw.``lg:w-02/04``
          Tw.``shadow-lg``
        ]
      ] [
        field "UserName" input [
          FormInputProp.InputProps [
            InputProp.Label "Email"
            InputProp.ConvertTo InputValue.Text
          ]
          FormInputProp.LeftIconClasses [ Fa.fa; Fa.``fa-mail-bulk`` ]
        ]

        field "Password" input [
          FormInputProp.InputProps [
            InputProp.Label "Password"
            InputProp.ConvertTo InputValue.Password
          ]
          FormInputProp.LeftIconClasses [ Fa.fa; Fa.``fa-lock``; Tw.``text-green-500`` ]
          FormInputProp.RightIconClasses [ Tw.``text-green-400`` ]
        ]

        field "Birthday" input [
          FormInputProp.InputProps [
            InputProp.Label "Birthday"
            InputProp.ConvertTo InputValue.Date
            InputProp.InputClasses [ Tw.``text-purple-500`` ]
          ]
        ]

        field "Roles" selector [
          SelectorProp.Label "Roles"
          SelectorProp.Source [ 1, "R1"; 2, "R2" ]
          SelectorProp.Displayer (fun (_,v) -> span [ Classes [ Tw.``ml-01`` ] ] [ str v ])
        ]

        field "Address.Country" input [
          FormInputProp.InputProps [
            InputProp.Label "Country"
            InputProp.ConvertTo InputValue.Text
          ]
          FormInputProp.RightIconClasses [ Fa.fa; Fa.``fa-map-marked`` ]
        ]

        field "Address.Street" input [
          FormInputProp.InputProps [
            InputProp.Label "Street"
            InputProp.ConvertTo InputValue.Text
          ]
          FormInputProp.LeftIconClasses [ Fa.fa; Fa.``fa-funnel-dollar``; Tw.``text-orange-400`` ]
          FormInputProp.RightIconClasses [ Fa.fa; Fa.``fa-passport``; Tw.``text-red-500`` ]
        ]

        field "Address.ZipCode" input [
          FormInputProp.InputProps [
            InputProp.Label "ZipCode"
            InputProp.ConvertTo InputValue.Number
          ]
        ]

        errorSummary state.UserProfileForm

        Layout.level [] [
          Button.primaryButton  "Submit" ignore
          Button.secondayButton "Cancel" ignore
        ]

        Icon.brand [
          Fa.``fa-github``
          Tw.block
          Tw.``bg-blue-200``
          Tw.``py-02``
          Tw.``text-center``
          Tw.``text-green-800``
        ]
      ]

      infoCard (sprintf "%A" (generateValueByForm typeof<UserProfile> UserProfile.defaultValue state.UserProfileForm))
        [
          Tw.``text-green-600``
          Tw.``bg-green-100``
          Tw.``sm:w-full``
          Tw.``lg:w-02/04``
        ]

      infoCard (sprintf "%A" UserProfile.defaultValue)
        [
          Tw.``text-gray-500``
          Tw.``bg-gray-100``
          Tw.``sm:w-full``
          Tw.``lg:w-02/04``
        ]
    ]
