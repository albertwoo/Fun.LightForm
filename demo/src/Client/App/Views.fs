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
       

let formInput: FieldRenderer =
    fun field dispatch ->
      input [
        Classes [
          tailwind.``focus:outline-none``
          tailwind.``focus:shadow-outline``
          tailwind.border
          tailwind.``border-gray-300``
          tailwind.``border-2``
          tailwind.``p-02``
          tailwind.rounded
          tailwind.``appearance-none``
        ]
        Type "email"
        Placeholder "test@mail.com"
        DefaultValue field.Value
        OnChange (fun e -> LightForm.Domain.OnChangeField (field.Name, e.Value) |> dispatch)
      ]


let app state dispatch =
    let formEl = formElement state.UserProfileForm (UserProfileMsg >> dispatch)

    div [
      Classes [
        tailwind.``font-sans``
        tailwind.``h-screen``
        tailwind.flex
        tailwind.``justify-center``
        tailwind.``items-center``
      ]
    ] [
      form [] [
        formEl "UserName" formInput
      ]
    ]
