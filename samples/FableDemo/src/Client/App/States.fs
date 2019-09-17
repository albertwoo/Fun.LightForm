module Client.App.States

open Elmish
open Fun.LightForm
open Fun.LightForm.Validators


let validators =
  Map.empty
  |> addValidations "UserName"
      [
        required "This field is reuqired"
        maxLength 30 "This field`s max length is 30"
      ]
  |> addValidations "Password"
      [
        required "Password is reuqired"
        maxLength 20 "This field`s max length is 20"
        (fun f -> if f.Value.ToString().Contains("password")
                  then Error [ "Cannot use passsord as password" ]
                  else Ok())
      ]
  |> addValidations "Birthday" [ dateValidator "Not a valid Date" ]
  |> addValidations "Roles"
      [
        seqMinLen 1 "Should at least contains one role"
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