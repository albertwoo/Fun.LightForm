[<AutoOpen>]
module Sample1.Forms

open Feliz
open Fun.LightForm

open type Html
open type prop


let fieldWrapper (tranErrors: 'Error list -> string) (ele: ReactElement) (field: FormField<'Error>) dispatch =
    div [
        children [
            ele
            match field.Errors with
            | [] -> ()
            | _ ->
                div [
                    text (tranErrors field.Errors)
                    classes [ Tw.``text-xs``; Tw.``text-red-400`` ]
                ]
        ]
    ]


let textInput tranErrors props (field: FormField<_>) dispatch =
    fieldWrapper
        tranErrors
        (
            input [
                yield! props
                type' "text"
                value (field.RawValue |> string)
                onChange (fun (s: string) -> LightFormMsg.ChangeField (field.Name, s) |> dispatch)
            ]
        )
        field
        dispatch


let numInput tranErrors props (field: FormField<_>) dispatch =
    fieldWrapper
        tranErrors
        (
            input [
                yield! props
                type' "number"
                value (field.RawValue |> string)
                onChange (fun (s: string) -> LightFormMsg.ChangeField (field.Name, s) |> dispatch)
            ]
        )
        field
        dispatch
