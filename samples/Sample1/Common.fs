[<AutoOpen>]
module Sample1.Common

open Zanaptak.TypedCssClasses


let [<Literal>] TailwindCssPath = __SOURCE_DIRECTORY__ + "/www/css/tailwind-generated.css"
type Tw = CssClasses<TailwindCssPath, Naming.Verbatim>

