module Fun.LightForm.FormViews

open System
open Fable.React
open Fable.React.Props


type FieldRenderer = FieldRenderer<Fable.React.ReactElement>


type InputType =
  | Email
  | Text
  | Number
  | Password
  | Date of format: string


type Fields() =
  static member field (state: LightForm) dispatch key (render: FieldRenderer) =
    state
    |> List.tryFind (fun x -> x.Name = key)
    |> Option.map (fun field ->
        fragment 
            [ FragmentProp.Key field.Name ] 
            [ render field dispatch ])
    |> Option.defaultValue 
        (div [ Style [ Color "red" ] ] 
             [ str (sprintf "Field is not configured for %s" key) ])

  static member fieldOuter(children, classes) =
    div
      [
        match classes with
        | Some cs -> yield Class cs
        | None    -> yield Style [ Margin "5px"; Padding "5px" ]
      ]
      children

  static member fieldLabel(label, classes) =
    div
      [
        match classes with
        | Some cs -> yield Class cs
        | None    -> yield Style [ Color "gray"; FontSize "0.9rem" ]
      ]
      [ str label ]

  static member fieldError(fieldValue, classes) =
      match fieldValue with
      | Valid _ -> []
      | Invalid (_, es) ->
           [
              for e in es ->
                div
                  [
                    match classes with
                    | Some cs -> yield Class cs
                    | None    -> yield Style [ Color "red"; FontSize "0.8rem"; Opacity "0.8" ]
                  ]
                  [ str e ]
           ]

  static member input
    (inputType: InputType
    ,?label: string
    ,?outerClasses: string
    ,?labelClasses: string
    ,?errorClasses: string
    ,?inputClasses: string
    ,?inputViewWrapper: ReactElement -> ReactElement)
    : FieldRenderer =
      fun field dispatch ->
        let inputView = 
          input
            [
              match inputClasses with
              | Some cs -> yield Class cs
              | None    -> yield Style [ Width "100%"; Margin "2px 0"; Padding "2px 5px"; BackgroundColor "#f1f1f1" ]
              yield Type (
                    match inputType with
                    | Email -> "email"
                    | Date _ -> "date"
                    | Text -> "text"
                    | Number -> "number"
                    | Password -> "password")
              yield DefaultValue (
                  let value = field |> getFormFieldValue
                  match inputType with
                  | Email
                  | Text
                  | Password
                  | Number -> if isNull value then box "" else box value
                  | Date f ->
                      try (value :?> DateTime).ToString(f) |> box
                      with _ -> box value)
              yield OnChange (fun e ->
                  match inputType with
                  | Email
                  | Text
                  | Password
                  | Number
                  | Date _ -> ChangeField (field.Name, e.Value)
                  |> dispatch)
            ]
        Fields.fieldOuter
          ([
            if label.IsSome then
              yield Fields.fieldLabel(label.Value, labelClasses) 

            match inputViewWrapper with
            | Some wrapper -> yield wrapper inputView
            | None         -> yield inputView

            yield! Fields.fieldError (field.Value, errorClasses)
          ]
         ,outerClasses)

  static member selector<'id, 'v when 'id : equality>
    (sourceList: ('id * 'v) list
    ,?displayer: ('id * 'v -> ReactElement)
    ,?label: string
    ,?outerClasses: string
    ,?labelClasses: string
    ,?errorClasses: string
    ,?inputClasses: string)
    : FieldRenderer =
      fun field disp ->
        let ids =
          try field |> getFormFieldValue :?> 'id seq
          with _ -> [||] :> 'id seq
        let createNewValue id =
          ids
          |> List.ofSeq
          |> List.partition (fun id' -> id = id')
          |> function
            | [], x -> id::x
            | _, x  -> x
          |> List.toArray
        Fields.fieldOuter
          ([
            if label.IsSome then
              yield Fields.fieldLabel(label.Value, labelClasses) 

            for (id, v) in sourceList do
              yield div
                [
                  Class (inputClasses |> Option.defaultValue "")
                ]
                [
                  yield input
                    [
                      Type "checkbox"
                      Value (ids |> Seq.exists ((=) id))
                      OnChange (fun _ -> ChangeField (field.Name, box (createNewValue id)) |> disp)
                    ]

                  match displayer with
                  | Some dis -> yield dis (id, v)
                  | None     -> yield v |> box |> string |> str
                ]

            yield! Fields.fieldError(field.Value, errorClasses) 
          ]
         ,outerClasses)
