[<AutoOpen>]
module Client.Controls.Common

open System
open Fable.React
open Fable.React.Props
open Zanaptak.TypedCssClasses


type Tw = CssClasses<"./public/css/tailwind-generated.css", Naming.Verbatim>
type Fa = CssClasses<"./public/css/font-awesome-v5-10-2.min.css", Naming.Verbatim>


let classes str = str |> List.filter (String.IsNullOrEmpty >> not) |> String.concat " "
let Classes = classes >> Class

let emptyView = div [ Style [ Display DisplayOptions.None ] ] []


[<RequireQualifiedAccess>]
module Heading =
  let h1 classes (txt: string) =
    div [
      Classes [
        yield Tw.``text-3xl``
        yield Tw.``font-bold``
        yield! classes
      ]
    ] [
      str txt 
    ]

  let h2 classes (txt: string) =
    div [
      Classes [
        yield Tw.``text-2xl``
        yield Tw.``font-semibold``
        yield! classes
      ]
    ] [
      str txt 
    ]


[<RequireQualifiedAccess>]
module Layout =
  let level classes childs =
    div [
      Classes [
        yield! classes
        yield Tw.``mx-auto``
        yield Tw.flex
        yield Tw.``flex-no-wrap``
        yield Tw.``items-center``
        yield Tw.``justify-center``
      ]
    ] childs

  let spacer classes =
    span [
      Classes [
        yield Tw.``px-02``
        yield! classes
      ]
    ] []


[<RequireQualifiedAccess>]
module Icon =
  let icon cs = span [ Classes cs ] []
  
  let brand classes =
    span [
      Classes [
        yield! classes
        yield Fa.fab
        yield Tw.``text-gray-800``
      ]
    ] []

  let solid classes =
    span [
      Classes [
        yield! classes
        yield Fa.fas
        yield Tw.``text-gray-800``
      ]
    ] []

  let urlIcon url icon =
    match url with
      | Some url ->
          a [
            Href url
          ] [
            icon
          ]
      | None ->
          emptyView


[<RequireQualifiedAccess>]
module Button =
  let button classes label onClick =
    button [
      OnClick onClick
      Classes [
        yield Tw.``m-02``
        yield Tw.``py-01``
        yield Tw.``px-02``
        yield Tw.rounded
        yield! classes
      ]
    ] [
      str label
    ]

  let primaryButton =
    button [
      Tw.``text-white``
      Tw.``bg-green-400``
      Tw.``hover:bg-green-500``
    ]

  let secondayButton =
    button [
      Tw.``text-gray-700``
      Tw.border
      Tw.``hover:bg-gray-200``
    ]
