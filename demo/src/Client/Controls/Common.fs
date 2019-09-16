[<AutoOpen>]
module Client.Controls.Common

open System
open Fable.React
open Fable.React.Props
open Zanaptak.TypedCssClasses


type tailwind = CssClasses<"./public/css/tailwind-generated.css", Naming.Verbatim>
type fontAwsome = CssClasses<"./public/css/font-awesome-v5-10-2.min.css", Naming.Verbatim>


let Classes str = str |> List.filter (String.IsNullOrEmpty >> not) |> String.concat " " |> Class


let emptyView = div [ Style [ Display DisplayOptions.None ] ] []


[<RequireQualifiedAccess>]
module Heading =
  let h1 classes (txt: string) =
    div [
      Classes [
        yield tailwind.``text-3xl``
        yield tailwind.``font-bold``
        yield! classes
      ]
    ] [
      str txt 
    ]

  let h2 classes (txt: string) =
    div [
      Classes [
        yield tailwind.``text-2xl``
        yield tailwind.``font-semibold``
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
        yield tailwind.``mx-auto``
        yield tailwind.flex
        yield tailwind.``flex-no-wrap``
        yield tailwind.``items-center``
        yield tailwind.``justify-center``
      ]
    ] childs

  let spacer classes =
    span [
      Classes [
        yield tailwind.``px-02``
        yield! classes
      ]
    ]


[<RequireQualifiedAccess>]
module Icons =
  let brand classes =
    span [
      Classes [
        yield! classes
        yield fontAwsome.fab
        yield tailwind.``text-gray-800``
      ]
    ] []

  let solid classes =
    span [
      Classes [
        yield! classes
        yield fontAwsome.fas
        yield tailwind.``text-gray-800``
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
