[<AutoOpen>]
module Fable.React.Extra

open Fable.React.Props


type HTMLPropExtra =
  /// Can only be used with </> or <//>
  | Classes of string list
  /// Can only be used with </> or <//>
  | Children of ReactElement list
  /// Can only be used with </> or <//>
  | Text of string
  interface IHTMLProp


/// Helper function to generate HTMLAttr.Class
let classes cs = cs |> String.concat " " |> Class


let private createReactElement elementF (props: IHTMLProp list) =
  let children, attrs =
    props
    |> List.partition (function
        | :? HTMLPropExtra -> true
        | _ -> false)
  let ps = children |> List.map (fun x -> x :?> HTMLPropExtra)
  elementF [
    yield! attrs
    yield ps |> UnionProps.concat (function HTMLPropExtra.Classes x -> Some x | _ -> None) |> classes |> unbox
  ] [
    yield! ps |> UnionProps.concat (function HTMLPropExtra.Children x -> Some x | _ -> None)
    
    match ps |> UnionProps.tryLast (function HTMLPropExtra.Text x -> Some x | _ -> None) with
      | Some x -> str x
      | None   -> ()
  ]

/// Create ReactElement with props which can be HTMLPropExtra
let (</>) elementF props     = createReactElement elementF props
/// Create ReactElement with a list of ReactElement to be contained in the parent ReactElement which can be HTMLPropExtra
let (<//>) elementF children = elementF </> [ HTMLPropExtra.Children children ]


/// An empty hidden div
let emptyView = div </> [ Style [ Display DisplayOptions.None ] ]
