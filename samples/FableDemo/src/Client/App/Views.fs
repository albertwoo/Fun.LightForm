module Client.App.Views

open Fable.React
open Fable.React.Props
open Fun.LightForm
open Client.Controls


let infoCard props =
    div </> [
        Classes [
            Tw.``text-gray-500``; Tw.``text-center``; Tw.``w-full``
            Tw.``py-02``; Tw.``px-01``; Tw.``m-02``; Tw.``shadow-lg``
        ]
        yield! props
    ]


let tabStyle isActive =
    [
        Tw.``w-01/02``; Tw.``text-center``; Tw.``bg-white``; Tw.``hover:bg-green-200``
        Tw.``py-02``; Tw.``cursor-pointer``
        if isActive then
            Tw.``bg-green-300``
            Tw.``font-semibold``
    ]


let app state dispatch =
    div </> [
        Classes [
            Tw.``font-sans``; Tw.flex; Tw.``flex-col``; Tw.``items-center``; Tw.``justify-center``
            Tw.``overflow-auto``; Tw.``py-04``
        ]
        Children [
            div </> [
              Classes [
                  Tw.``sm:w-full``; Tw.``lg:w-02/04``; Tw.``shadow-lg``
                  Tw.flex; Tw.``flex-col``; Tw.``overflow-hidden``
              ]
              Children [
                  div </> [
                      Classes [
                          Tw.flex; Tw.``flex-row``; Tw.``flex-no-wrap``; Tw.``items-stretch``
                          Tw.``w-full``; Tw.``bg-red-100``; Tw.``shadow-md``
                      ]
                      Children [
                          div </> [
                              Text "Map form"
                              OnClick (fun _ -> SwitchTab ActiveTab.UserProfileForm |> dispatch)
                              Classes (tabStyle (match state.ActiveTab with ActiveTab.UserProfileForm -> true | _ -> false))
                          ]
                          div </> [
                              Text "Value form"
                              OnClick (fun _ -> SwitchTab ActiveTab.UserProfileValueFrom |> dispatch)
                              Classes (tabStyle (match state.ActiveTab with ActiveTab.UserProfileValueFrom -> true | _ -> false))
                          ]
                          div </> [
                              Text "Map form by functional component"
                              OnClick (fun _ -> SwitchTab ActiveTab.UserProfileFromByFn |> dispatch)
                              Classes (tabStyle (match state.ActiveTab with ActiveTab.UserProfileFromByFn -> true | _ -> false))
                          ]
                      ]
                  ]

                  UserProfileForm.render state dispatch

                  Layout.level [
                      Children [
                          Button.primaryButton  [ Text "Submit" ]
                          Button.secondayButton [ Text "Cancel" ]
                      ]
                  ]

                  Icon.brand [
                      Classes [
                          Fa.``fa-github``; Tw.block; Tw.``bg-blue-200``
                          Tw.``py-02``; Tw.``text-center``; Tw.``text-green-800``
                      ]
                  ]
                ]
            ]

            infoCard [
                Classes [ Tw.``text-green-600``; Tw.``bg-green-100``; Tw.``sm:w-full``; Tw.``lg:w-02/04`` ]
                Children [
                    str (sprintf "%A" (generateValueByForm UserProfile.defaultValue state.UserProfileForm))
                    hr []
                    str (sprintf "%A" (tryGenerateValueByForm<UserProfile> state.UserProfileForm))
                    hr []
                    str (sprintf "%A" state.UserProfileValueFrom.Value)
                ]
            ]

            infoCard [
                Classes [ Tw.``text-gray-500``; Tw.``bg-gray-100``; Tw.``sm:w-full``; Tw.``lg:w-02/04`` ]
                Text (sprintf "%A" UserProfile.defaultValue)
            ]
        ]
    ]
