module Client.App.UserProfileForm

open Fable.React
open Fable.React.Props
open Fun.LightForm
open Fun.LightForm.FormView
open Client.Controls
open Client.Controls.Form


let render state dispatch =
    let createFields fnField =
        let formFields =
            match state.ActiveTab with
            | ActiveTab.UserProfileForm       -> state.UserProfileForm
            | ActiveTab.UserProfileValueFrom  -> generateFromByValueForm state.UserProfileValueFrom
            | ActiveTab.UserProfileFromByFn   -> []
        let field key renderer arg =
            match state.ActiveTab with
            | ActiveTab.UserProfileForm       -> Form.field formFields (UserProfileMsg >> dispatch) key (renderer arg)
            | ActiveTab.UserProfileValueFrom  -> Form.field formFields (UserProfileValueFromMsg >> dispatch) key (renderer arg)
            | ActiveTab.UserProfileFromByFn   -> match fnField with
                                                 | Some f -> f key (renderer arg)
                                                 | None -> div </> [ Text "No fnField provided" ]
        [
            field "IsPublish" selector [
                SelectorProp.Source [ true, "Publish" ]
            ]

            field "IsPublish" selector [
                SelectorProp.Source [ false, "Unpublish" ]
            ]

            field "UserName" input [
                InputProp.Label "Email"
                InputProp.ConvertTo InputValue.Text
                InputProp.LeftView (Icon.simpleIcon [ Fa.fa; Fa.``fa-mail-bulk`` ])
            ]

            field "Password" input [
                InputProp.Label "Password"
                InputProp.ConvertTo InputValue.Password
                InputProp.LeftView (Icon.simpleIcon [ Fa.fa; Fa.``fa-lock``; Tw.``text-green-500`` ])
            ]

            field "Birthday" input [
                InputProp.Label "Birthday"
                InputProp.ConvertTo InputValue.Date
                InputProp.InputAttrs [ Classes [ Tw.``text-purple-500`` ] ]
            ]

            field "OptionTest1" input [
                InputProp.Label "OptionTest1"
                InputProp.ConvertTo InputValue.Text
                InputProp.InputAttrs [ Classes [ Tw.``text-purple-500`` ] ]
            ]
            field "OptionTest2" input [
                InputProp.Label "OptionTest2"
                InputProp.ConvertTo InputValue.Number
                InputProp.InputAttrs [ Classes [ Tw.``text-purple-500`` ] ]
            ]
            field "OptionTest3" input [
                InputProp.Label "OptionTest3"
                InputProp.ConvertTo InputValue.Date
                InputProp.InputAttrs [ Classes [ Tw.``text-purple-500`` ] ]
            ]

            field "Roles" selector [
                SelectorProp.Label "Roles"
                SelectorProp.Source [ 1, "R1"; 2, "R2" ]
                SelectorProp.OnlyOne false
            ]

            field "Roles" selector [
                SelectorProp.Label "Roles"
                SelectorProp.Source [ 1, "R1"; 2, "R2" ]
                SelectorProp.OnlyOne false
                SelectorProp.EnableDropdown true
            ]

            field "DefaultRole" selector [
                SelectorProp.Label "Default Role"
                SelectorProp.Source [ 1, "R1"; 2, "R2" ]
                SelectorProp.OnlyOne true
            ]

            field "Country" selector [
                SelectorProp.Label "Selected Country"
                SelectorProp.OnlyOne true
                SelectorProp.AtLeastOne true
                SelectorProp.Source [
                for i in 1..100 -> i, sprintf "Country %d" i
                ]
                SelectorProp.SelectionsContainerAttrs [
                Style [
                    MaxHeight "100px"
                    OverflowY OverflowOptions.Auto
                ]
                ]
            ]

            field "Country" selector [
                SelectorProp.Label "Selected Country"
                SelectorProp.OnlyOne true
                SelectorProp.AtLeastOne true
                SelectorProp.EnableDropdown true
                SelectorProp.Source [
                    for i in 1..100 -> i, sprintf "Country %d" i
                ]
                SelectorProp.SelectionsContainerAttrs [
                    Style [
                        MaxHeight "200px"
                        OverflowY OverflowOptions.Auto
                    ]
                ]
            ]

            field "Address.Country" input [
                InputProp.Label "Country"
                InputProp.AlwaysRerender true
                InputProp.ConvertTo InputValue.Text
                InputProp.RightView (Icon.simpleIcon [ Fa.fa; Fa.``fa-map-marked`` ])
            ]

            field "Address.Street" input [
                InputProp.Label "Street"
                InputProp.ConvertTo InputValue.Text
                InputProp.LeftView (Icon.simpleIcon [ Fa.fa; Fa.``fa-funnel-dollar``; Tw.``text-orange-400`` ])
                InputProp.RightView (Icon.simpleIcon [ Fa.fa; Fa.``fa-passport``; Tw.``text-red-500`` ])
            ]

            field "Address.ZipCode" input [
                InputProp.Label "ZipCode"
                InputProp.ConvertTo InputValue.Number
            ]

            field "Description" textArea [
                TextAreaProp.Label "Description"
            ]
        ]

    div </> [
        Key (string state.ActiveTab)
        Children [
            match state.ActiveTab with
            | ActiveTab.UserProfileForm
            | ActiveTab.UserProfileValueFrom ->
                div [] (createFields None)
            | ActiveTab.UserProfileFromByFn ->
                lightForm [
                    LightFormProp.InitForm (state.UserProfileValueFrom |> generateFromByValueForm)
                    LightFormProp.Validators States.validators
                    LightFormProp.OnFieldChange (fun x -> Browser.Dom.console.error x)
                    LightFormProp.OnFormChanged (FormUtils.tryGenerateValueByForm<UserProfile> >> fun x -> Browser.Dom.console.error x)
                    LightFormProp.CreateFields (Some >> createFields)
                ]

            match state.ActiveTab with
            | ActiveTab.UserProfileForm -> errorSummary state.UserProfileForm
            | ActiveTab.UserProfileValueFrom -> state.UserProfileValueFrom |> generateFromByValueForm |> errorSummary
            | ActiveTab.UserProfileFromByFn -> nothing
        ]
    ]
