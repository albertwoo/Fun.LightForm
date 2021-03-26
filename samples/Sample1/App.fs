module Sample1.App

open Feliz
open Browser

open type Html
open type prop


[<RequireQualifiedAccess>]
type Tab =
    | UserProfile
    | MultiFormsDemo


[<ReactComponent>]
let App() =
    let tab, setTab = React.useState Tab.UserProfile

    div [
        div [
            classes [ Tw.flex; Tw.``flex-row``; Tw.``items-stretch``; Tw.``cursor-pointer`` ]
            children [
                div [
                    text (nameof Tab.UserProfile)
                    classes [ Tw.``bg-green-100``; Tw.``text-center``; Tw.``hover:bg-green-200``; Tw.``py-2``; Tw.``w-full`` ]
                    onClick (fun _ -> setTab Tab.UserProfile)
                ]
                div [
                    text (nameof Tab.MultiFormsDemo)
                    classes [ Tw.``bg-blue-100``; Tw.``text-center``; Tw.``hover:bg-blue-200``; Tw.``py-2``; Tw.``w-full`` ]
                    onClick (fun _ -> setTab Tab.MultiFormsDemo)
                ]
            ]
        ]
        match tab with
        | Tab.UserProfile -> UserProfile()
        | Tab.MultiFormsDemo -> MultiFormsDemo()
    ]


ReactDOM.render(App(), document.getElementById "root")
