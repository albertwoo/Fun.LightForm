[<AutoOpen>]
module Sample1.UserProfile

open System
open Feliz
open Fun.LightForm
open Fun.LightForm.NestFrom

open type Html
open type prop


type Profile =
    { UserName: string
      Password: string
      Birthday: DateTime
      OptionTest1: string option
      OptionTest2: int option
      OptionTest3: DateTime option
      Roles: int list
      DefaultRole: int
      Country: int option
      Address: Address
      IsPublish: bool
      Description: string option }

    static member defaultValue =
        { UserName = "email"
          Password = "password"
          Birthday = DateTime.Now
          OptionTest1 = None
          OptionTest2 = None
          OptionTest3 = None
          Roles = []
          DefaultRole = -1
          Address = Address.defaultValue
          Country = None
          IsPublish = true
          Description = None }

and Address =
    { Country: string
      City: string
      Street: string option
      ZipCode: int }

    static member defaultValue =
        { Country = "shanghai"
          City = "shanghai"
          Street = None
          ZipCode = 12 }


type UserProfileError =
    | NameIsRequired
    | CannotBeAdmin
    | InvalidZipCode of float
    | TooLong of int


let tranError = function
    | UserProfileError.CannotBeAdmin -> "Cannot be admin"
    | UserProfileError.InvalidZipCode x -> "invalid zip code"
    | UserProfileError.NameIsRequired -> "Name is required"
    | UserProfileError.TooLong x -> $"Name`s length cannot exceed {x}"


let validators =
    Validation.emptyValidators
    |> Validation.addValidators (nameof Profile.defaultValue.UserName) [ 
        Validation.required NameIsRequired
        Validation.not "admin" CannotBeAdmin
        Validation.maxLength 10 TooLong
    ]
    |> Validation.addValidators (nameof Profile.defaultValue.Address <.> nameof Profile.defaultValue.Address.ZipCode) [
        Validation.minNum 100. InvalidZipCode
    ]


[<ReactComponent>]
let UserProfile () =
    let defaultProfile = Profile.defaultValue

    let form, setForm = LightForm.useLightForm(defaultProfile, validators)
    let field fieldName render args = form.CreateField fieldName (render args)
    let tranErrors = List.map tranError >> String.concat ","

    div [
        classes [ Tw.rounded; Tw.``shadow-lg``; Tw.``p-4``; Tw.``overflow-auto``; Tw.``w-screen`` ]
        children [        
            button [
                text "Reset"
                classes [ Tw.rounded; Tw.``bg-purple-200``; Tw.``hover:bg-purple-400``; Tw.``cursor-pointer``; Tw.``px-2`` ]
                onClick (fun _ -> setForm(defaultProfile, validators))
            ]

            field (nameof defaultProfile.UserName) (textInput tranErrors) [
                classes [ Tw.``bg-gray-100``; Tw.``rounded-lg``; Tw.``px-2``; Tw.``py-1``; Tw.``m-1``; Tw.``w-full`` ]
            ]
            field (nameof defaultProfile.Address + "." + nameof defaultProfile.Address.ZipCode) (numInput tranErrors) [
                type' "number"
                classes [ Tw.``bg-green-100``; Tw.``rounded-lg``; Tw.``px-2``; Tw.``py-1``; Tw.``m-1``; Tw.``w-full`` ]
            ]

            div [
                classes [ Tw.``text-xs``; Tw.``text-red-400``; Tw.rounded; Tw.``bg-red-100`` ]
                children [
                    for error in form.Errors do
                        match error with
                        | UserProfileError.CannotBeAdmin -> div "Cannot be admin"
                        | UserProfileError.InvalidZipCode x -> div "invalid zip code"
                        | UserProfileError.NameIsRequired -> div "Name is required"
                        | UserProfileError.TooLong x -> div $"Name`s length cannot exceed {x}"
                ]
            ]

            if form.IsModified then
                div [
                    text "Form is changed"
                    classes [ ]
                ]

            div [
                classes [ Tw.rounded; Tw.``bg-green-100``; Tw.``p-4``; Tw.``my-4`` ]
                text (form.GetValue() |> string)
            ]
            div [
                classes [ Tw.rounded; Tw.``bg-blue-100``; Tw.``p-4``; Tw.``my-4`` ]
                text (form.Form |> string)
            ]
        ]
    ]
