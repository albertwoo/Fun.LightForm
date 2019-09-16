module Client.App.Views

open System
open Fable.React
open Fable.React.Props
open Zanaptak.TypedCssClasses
open Fun

type tailwind = CssClasses<"./public/css/tailwind-generated.css", Naming.Verbatim>
type fontAwsome = CssClasses<"./public/css/font-awesome-v5-10-2.min.css", Naming.Verbatim>


let Classes str = str |> List.filter (String.IsNullOrEmpty >> not) |> String.concat " " |> Class

let emptyView = div [ Style [ Display DisplayOptions.None ] ] []



type FieldRenderer = LightForm.Domain.FormField -> (LightForm.Domain.Msg -> unit) -> Fable.React.ReactElement

let formElement (state: LightForm.Domain.Model) dispatch key (render: FieldRenderer) =
    state
    |> List.tryFind (fun x -> x.Name = key)
    |> Option.map (fun field ->
        fragment 
            [ FragmentProp.Key field.Name ] 
            [ render field dispatch ])
    |> Option.defaultValue 
        (div [ Style [ Color "red" ] ] 
             [ str (sprintf "Field is not configured for %s" key) ])


let formOuter children =
  div [
    Classes [
      yield tailwind.border
      yield tailwind.``border-gray-200``
      yield tailwind.``border-2``
      yield tailwind.``p-02``
      yield tailwind.``m-02``
      yield tailwind.rounded
      yield tailwind.``appearance-none``
      yield tailwind.``w-full``
    ]
  ] children

let formLabel label =
  div [
    Classes [
      tailwind.``text-gray-600``
      tailwind.``pb-01``
    ]
  ] [
    str label
  ]

let formError state =
  match state with
  | LightForm.Domain.Valid -> [ ]
  | LightForm.Domain.Invalid es ->
       [
        for e in es ->
          div [
            Classes [
              tailwind.``text-red-400``
              tailwind.``mt-01``
              tailwind.``text-sm``
            ]
          ] [
            str e
          ]
       ]


type InputType =
  | Email
  | Text
  | Number
  | Password
  | DateTime of format: string

let formInput ty label classes: FieldRenderer =
    fun field dispatch ->
      formOuter [
        yield formLabel label

        yield input [
          Classes [
            yield tailwind.``focus:outline-none``
            yield tailwind.``focus:shadow-outline``
            yield tailwind.``border-b-2``
            yield tailwind.``w-full``
            yield! classes
          ]
          Type (
               match ty with
               | Email -> "email"
               | DateTime _
               | Text -> "text"
               | Number -> "number"
               | Password -> "password"
               )
          DefaultValue (
              match ty with
              | Email
              | Text
              | Password
              | Number -> string field.Value
              | DateTime f -> (field.Value :?> DateTime).ToString(f)
              )
          OnChange (fun e ->
              match ty with
              | Email
              | Text
              | Password
              | Number -> e.Value |> box
              | DateTime _ -> DateTime.Parse e.Value |> box
              |> fun v -> LightForm.Domain.OnChangeField (field.Name, v)
              |> dispatch)
        ]

        yield! formError field.ValidationState
      ]


let formSelects label (sourceList: ('id * 'v) list) (displayer: 'id * 'v -> ReactElement): FieldRenderer =
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
    formOuter [
      yield formLabel label

      for (id, v) in sourceList do
        yield div [
            Classes [
              tailwind.flex
              tailwind.``items-center``
            ]
          ] [
            input [
              Type "checkbox"
              Value (ids |> Seq.exists ((=) id))
              OnChange (fun _ -> LightForm.Domain.OnChangeField (f.Name, box (createNewValue id)) |> disp)
              Classes [
                tailwind.``ml-02``
                tailwind.``mr-01``
              ]
            ]
            displayer (id, v)
          ]

      yield! formError f.ValidationState
    ]


let app state dispatch =
    let formEl = formElement state.UserProfileForm (UserProfileMsg >> dispatch)

    div [
      Classes [
        tailwind.``font-sans``
        tailwind.``h-screen``
        tailwind.flex
        tailwind.``flex-wrap``
        tailwind.``justify-center``
        tailwind.``items-center``
      ]
    ] [
      form [] [
        formEl "UserName"         (formInput InputType.Email "Email" [  ])
        formEl "Password"         (formInput InputType.Password "Password" [])
        formEl "Birthday"         (formInput (InputType.DateTime "yyyy/MM/dd") "Birthday" [])
        formEl "Roles"            (formSelects "Roles" [ 1, "R1"; 2, "R2" ] (fun (_,v) -> str v))
        formEl "Address:Country"  (formInput InputType.Text "Country" [])
      ]

      div [] [
        str (sprintf "%A" state.UserProfileForm)
      ]
    ]
