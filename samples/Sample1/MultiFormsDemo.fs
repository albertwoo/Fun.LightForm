[<AutoOpen>]
module Sample1.MultiFormsDemo

open System
open Feliz
open Fun.LightForm

open type Html
open type prop



let private addresses =
    [
        { Address.defaultValue with Country = "China" }
        { Address.defaultValue with Country = "USA" }
        { Address.defaultValue with Country = "Swiss" }
    ]

let validators =
    Map.empty
    |> Validation.addValidators (nameof Profile.defaultValue.Address.ZipCode) [
        Validation.minNum 100. (sprintf "InvalidZipCode %A")
    ]


[<ReactComponent>]
let MultiFormsDemo () =
    let defaultAddress = Address.defaultValue
    let forms = LightForm.useLightForms(addresses, (fun x -> x.Country), (fun _ -> validators))
    let field key field render args = forms.CreateField key field (render args)
    
    div [
        classes [ Tw.rounded; Tw.``shadow-lg``; Tw.``p-4``; Tw.``overflow-auto``; Tw.``w-screen`` ]
        children [
            for country in [ "China"; "USA"; "Swiss" ] do
                div [
                    classes [ Tw.``bg-blue-100``; Tw.rounded; Tw.``my-4`` ]
                    children [
                        div [
                            text country
                        ]
                        field country (nameof defaultAddress.City) (textInput string) [
                            classes [ Tw.``bg-gray-100``; Tw.``rounded-lg``; Tw.``px-2``; Tw.``py-1``; Tw.``m-1``; Tw.``w-full`` ]
                        ]
                        field country (nameof defaultAddress.ZipCode) (numInput string) [
                            classes [ Tw.``bg-gray-100``; Tw.``rounded-lg``; Tw.``px-2``; Tw.``py-1``; Tw.``m-1``; Tw.``w-full`` ]
                        ]
                    ]
                ]

            div [
                classes [ Tw.rounded; Tw.``bg-green-100``; Tw.``p-4``; Tw.``my-4`` ]
                text (forms.GetValues() |> string)
            ]
            div [
                classes [ Tw.rounded; Tw.``bg-blue-100``; Tw.``p-4``; Tw.``my-4`` ]
                text (forms.Forms |> string)
            ]
        ]
    ]

