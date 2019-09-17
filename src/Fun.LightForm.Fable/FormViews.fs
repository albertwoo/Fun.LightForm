module Fun.LightForm.FormViews

open System
open Fable.React
open Fable.React.Props


type FieldRenderer = FieldRenderer<Fable.React.ReactElement>


let formElement (state: LightForm) dispatch key (render: FieldRenderer) =
    state
    |> List.tryFind (fun x -> x.Name = key)
    |> Option.map (fun field ->
        fragment 
            [ FragmentProp.Key field.Name ] 
            [ render field dispatch ])
    |> Option.defaultValue 
        (div [ Style [ Color "red" ] ] 
             [ str (sprintf "Field is not configured for %s" key) ])


let fieldOuter classes children = div [ Class classes ] children

let fieldLabel classes label = div [ Class classes ] [ str label ]

let fieldError classes fieldValue =
    match fieldValue with
    | Valid _ -> []
    | Invalid (_, es) ->
         [
            for e in es -> div [ Class classes ] [ str e ]
         ]


type InputType =
  | Email
  | Text
  | Number
  | Password
  | DateTime of format: string

type InputProp =
  { OuterClasses: string
    LabelClasses: string
    ErrorClasses: string
    InputClasses: string
    Type: InputType
    Label: string }

let inputField (prop: InputProp): FieldRenderer =
    fun field dispatch ->
      fieldOuter prop.OuterClasses [
        yield fieldLabel prop.LabelClasses prop.Label

        yield input [
          Class prop.InputClasses
          Type (
               match prop.Type with
               | Email -> "email"
               | DateTime _
               | Text -> "text"
               | Number -> "number"
               | Password -> "password"
               )
          DefaultValue (
              let value = field |> getFormFieldValue
              match prop.Type with
              | Email
              | Text
              | Password
              | Number -> string value
              | DateTime f ->
                  try (value :?> DateTime).ToString(f)
                  with _ -> string value
              )
          OnChange (fun e ->
              match prop.Type with
              | Email
              | Text
              | Password
              | Number
              | DateTime _ -> ChangeField (field.Name, e.Value)
              |> dispatch)
        ]

        yield! fieldError prop.ErrorClasses field.Value
      ]


type SelectsProp<'id, 'v> =
  { OuterClasses: string
    LabelClasses: string
    ErrorClasses: string
    InputClasses: string
    Label: string
    SourceList: ('id * 'v) list
    Displayer: 'id * 'v -> ReactElement }

let selectsField (prop: SelectsProp<_, _>): FieldRenderer =
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
      fieldOuter prop.OuterClasses [
        yield fieldLabel prop.LabelClasses prop.Label

        for (id, v) in prop.SourceList do
          yield div [
              Class prop.InputClasses
            ] [
              input [
                Type "checkbox"
                Value (ids |> Seq.exists ((=) id))
                OnChange (fun _ -> ChangeField (field.Name, box (createNewValue id)) |> disp)
                Class prop.InputClasses
              ]
              prop.Displayer (id, v)
            ]

        yield! fieldError prop.ErrorClasses field.Value
      ]
