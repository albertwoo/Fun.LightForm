namespace Fun.LightForm.FormView

open Fable.React
open Fable.React.Props
open Fun.LightForm


[<RequireQualifiedAccess>]
type LightFormProp =
    | InitForm of LightForm
    | Validators of Map<FieldKey, Validator list>
    | OnFieldChange of (LightFormMsg -> unit)
    | OnFormChanged of (LightForm -> unit)
    | FormAttrs of IHTMLProp list
    | CreateFields of ((FieldKey -> FieldRenderer<ReactElement> -> ReactElement) -> ReactElement list)


[<AutoOpen>]
module Helpers =
    let lightForm =
        FunctionComponent.Of(
            fun (props: LightFormProp list) ->
                let initForm = props |> UnionProps.tryLast (function LightFormProp.InitForm x -> Some x | _ -> None) |> Option.defaultValue []
                let validators = props |> UnionProps.tryLast (function LightFormProp.Validators x -> Some x | _ -> None) |> Option.defaultValue Map.empty
                let createFields = props |> UnionProps.tryLast (function LightFormProp.CreateFields x -> Some x | _ -> None)
                let onFieldChanges = props |> UnionProps.concat (function LightFormProp.OnFieldChange x -> Some [x] | _ -> None)
                let onFormChangeds = props |> UnionProps.concat (function LightFormProp.OnFormChanged x -> Some [x] | _ -> None)

                let reducer =
                    Hooks.useReducer(
                        (fun state msg ->
                            onFieldChanges |> List.iter (fun f -> f msg)
                            let newState = updateFormWithMsg validators msg state
                            onFormChangeds |> List.iter (fun f -> f newState)
                            newState)
                        ,updateFormWithValidators validators initForm
                    )

                form </> [
                    yield! props |> UnionProps.concat (function LightFormProp.FormAttrs x -> Some x | _ -> None)
                    Children [
                        match createFields with
                        | Some f -> yield! f (Form.field reducer.current reducer.update)
                        | None -> nothing
                    ]
                ]
        )
