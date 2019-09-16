module Fun.LightForm.Views

open System
open Fable.React
open Fable.React.Props
open Domain


type FieldRenderer = FormField -> (Msg -> unit) -> Fable.React.ReactElement

let formElement (state: Model) dispatch key (render: FieldRenderer) =
    state
    |> List.tryFind (fun x -> x.Name = key)
    |> Option.map (fun field ->
        fragment 
            [ FragmentProp.Key field.Name ] 
            [ render field dispatch ])
    |> Option.defaultValue 
        (div [ Style [ Color "red" ] ] 
             [ str (sprintf "Field is not configured for %s" key) ])


let formOuter classes children = div [ Class classes ] children

let formLabel classes label = div [ Class classes ] [ str label ]

let formError classes state =
  match state with
  | Valid -> [ ]
  | Invalid es ->
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

let formInput (prop: InputProp): FieldRenderer =
    fun field dispatch ->
      formOuter prop.OuterClasses [
        yield formLabel prop.LabelClasses prop.Label

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
              match prop.Type with
              | Email
              | Text
              | Password
              | Number -> string field.Value
              | DateTime f -> (field.Value :?> DateTime).ToString(f)
              )
          OnChange (fun e ->
              match prop.Type with
              | Email
              | Text
              | Password
              | Number -> e.Value |> box
              | DateTime _ -> DateTime.Parse e.Value |> box
              |> fun v -> OnChangeField (field.Name, v)
              |> dispatch)
        ]

        yield! formError prop.ErrorClasses field.ValidationState
      ]


type SelectsProp<'id, 'v> =
  { OuterClasses: string
    LabelClasses: string
    ErrorClasses: string
    InputClasses: string
    Label: string
    SourceList: ('id * 'v) list
    Displayer: 'id * 'v -> ReactElement }

let formSelects (prop: SelectsProp<_, _>): FieldRenderer =
  fun f disp ->
    let ids =
      try f.Value :?> 'id seq
      with _ -> [||] :> 'id seq
    let createNewValue id =
      ids
      |> List.ofSeq
      |> List.partition (fun id' -> id = id')
      |> function
        | [], x -> id::x
        | _, x  -> x
      |> List.toArray
    formOuter prop.OuterClasses [
      yield formLabel prop.LabelClasses prop.Label

      for (id, v) in prop.SourceList do
        yield div [
            Class prop.InputClasses
          ] [
            input [
              Type "checkbox"
              Value (ids |> Seq.exists ((=) id))
              OnChange (fun _ -> OnChangeField (f.Name, box (createNewValue id)) |> disp)
              Class prop.InputClasses
            ]
            prop.Displayer (id, v)
          ]

      yield! formError prop.ErrorClasses f.ValidationState
    ]
