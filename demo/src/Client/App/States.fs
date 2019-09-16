module Client.App.States

open Elmish
open Fun
open Fun.LightForm.Validators


let validators =
  Map.empty
  |> addValidations "UserName"
      [
        required "This field is reuqired"
        maxLength "This field`s max length is 30" 30
      ]
  |> addValidations "Password"
      [
        required "Password is reuqired"
        maxLength "This field`s max length is 20" 20
        (fun f -> if f.Value.ToString().Contains("password")
                  then LightForm.Domain.Invalid [ "Cannot use passsord as password" ]
                  else LightForm.Domain.Valid)
      ]
  |> addValidations "Roles"
      [
        listMinLen "Should at least contains one role" 1
      ]


let init() =
  { UserProfileForm =
      LightForm.Utils.generateFormByValue typeof<UserProfile> UserProfile.defaultValue
      |> LightForm.States.validateForm validators
      |> fst }
  , Cmd.none


let update msg (state: State) =
    match msg with
    | UserProfileMsg msg' ->
        { state with
            UserProfileForm = LightForm.States.update validators msg' state.UserProfileForm }
        , Cmd.none