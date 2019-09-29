module Client.App.States

open Elmish
open Fun.LightForm
open Fun.LightForm.Validators


let validators =
  Map.empty
  |> addValidators "UserName"
      [
        required "This field is reuqired"
        maxLength 10 "This field`s max length is 10"
      ]
  |> addValidators "Password"
      [
        required "Password is reuqired"
        maxLength 20 "This field`s max length is 20"
        (fun f -> if f.Value.ToString().Contains("password")
                  then Error [ "Passsord cannot contain password" ]
                  else Ok())
      ]
  |> addValidators "Birthday" [ dateValidator "Not a valid Date" ]
  |> addValidators "Roles"
      [
        seqMinLen 1 "Should at least contains one role"
      ]
  |> addValidators "DefaultRole" [ required "Default role is required" ]
  |> addValidators "Country" [ required "Country role is required" ]
  |> addValidators "Address.ZipCode"
      [
        maxNum 12. "Should not bigger than 12"
      ]


let init() =
  { UserProfileForm =
      generateFormByValue typeof<UserProfile> UserProfile.defaultValue
      |> updateFormWithValidators validators }
  , Cmd.none


let update msg (state: State) =
  match msg with
  | UserProfileMsg msg' ->
      { state with
          UserProfileForm = updateFormWithMsg validators msg' state.UserProfileForm }
      , Cmd.none