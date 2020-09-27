[<AutoOpen>]
module Fable.React.Extra

open Fable.React.Props


/// Can only be used with </>
type HTMLPropExtra =
    /// Can only be used with </>
    /// Concat all the values together and override previous Class value
    | Classes of string list
    /// Can only be used with </>
    /// Concat all the values together and override previous Style value
    | Styles of CSSProp list
    /// Can only be used with </>
    /// Concat all the values together
    | Children of ReactElement list
    /// Can only be used with </>
    /// The last value will be the only child for the parent element
    | Text of string
    interface IHTMLProp


/// Helper function to generate HTMLAttr.Class
let classes cs = cs |> String.concat " " |> Class


let private createReactElement elementF (props: IHTMLProp seq) =
    let extras, attrs =
        props
        |> Seq.fold
            (fun (extras, attrs) x ->
              match x with
              | :? HTMLPropExtra as x -> extras@[x], attrs
              | x -> extras, attrs@[x])
            ([], [])

    let cs = extras |> UnionProps.concat (function HTMLPropExtra.Classes x -> Some x | _ -> None)
    let ss = extras |> UnionProps.concat (function HTMLPropExtra.Styles x -> Some x | _ -> None)

    elementF [
            yield! attrs
            if cs |> Seq.length > 0 then classes cs
            if ss |> Seq.length > 0 then Style ss
        ][
            match extras |> UnionProps.tryLast (function HTMLPropExtra.Text x -> Some x | _ -> None) with
            | Some x -> str x
            | None   ->
                yield! extras |> Seq.choose (function HTMLPropExtra.Children x -> Some x | _ -> None) |> Seq.concat
    ]

/// Create ReactElement with props which can be HTMLPropExtra
let (</>) elementF props = createReactElement elementF props
/// Create ReactElement with a list of ReactElement to be contained in the parent ReactElement which can be HTMLPropExtra


/// An empty hidden div
let emptyView = nothing // div </> [ Style [ Display DisplayOptions.None ] ]
