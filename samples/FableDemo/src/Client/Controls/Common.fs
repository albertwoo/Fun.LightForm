[<AutoOpen>]
module Client.Controls.Common

open Fable.React
open Fable.React.Props
open Zanaptak.TypedCssClasses


type Tw = CssClasses<"./public/css/tailwind-generated.css", Naming.Verbatim>
type Fa = CssClasses<"./public/css/font-awesome-v5-10-2.min.css", Naming.Verbatim>


[<RequireQualifiedAccess>]
module Heading =
  let h1 props =
    div </> [
      Classes [
        Tw.``text-3xl``
        Tw.``font-bold``
      ]
      yield! props
    ]

  let h2 props =
    div </> [
      Classes [
        Tw.``text-2xl``
        Tw.``font-semibold``
      ]
      yield! props
    ]


[<RequireQualifiedAccess>]
module Layout =
  let level props =
    div </> [
      yield! props
      Classes [
        Tw.``mx-auto``
        Tw.flex
        Tw.``flex-no-wrap``
        Tw.``items-center``
        Tw.``justify-center``
      ]
    ]

  let spacer classes =
    span </> [
      Classes [
        Tw.``px-02``
        yield! classes
      ]
    ]


[<RequireQualifiedAccess>]
module Icon =
  let icon props = span </> props

  let simpleIcon cs = icon [ Classes cs ]

  let brand props =
    icon [
      Classes [
        Fa.fab
        Tw.``text-gray-800``
      ]
      yield! props
    ]

  let solid props =
    icon [
      Classes [
        Fa.fas
        Tw.``text-gray-800``
      ]
      yield! props
    ]

  let urlIcon url icon =
    match url with
      | Some url ->
          a </> [
            Href url
            Children [ icon ]
          ]
      | None ->
          emptyView


[<RequireQualifiedAccess>]
module Button =
  let button props =
    button </> [
      Classes [
        Tw.``m-02``
        Tw.``py-01``
        Tw.``px-02``
        Tw.rounded
      ]
      yield! props
    ]

  let primaryButton props =
    button [
      Classes [
        Tw.``text-white``
        Tw.``bg-green-400``
        Tw.``hover:bg-green-500``
      ]
      yield! props
    ]

  let secondayButton props =
    button [
      Classes [
        Tw.``text-gray-700``
        Tw.border
        Tw.``hover:bg-gray-200``
      ]
      yield! props
    ]
