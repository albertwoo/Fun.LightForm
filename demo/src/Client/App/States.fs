module Client.App.States

open Elmish
open Fun

let defaultState =
  { UserProfileForm = LightForm.Utils.generateFormByValue UserProfile.defaultValue }


let init() =
  defaultState
  , Cmd.none


let update msg (state: State) =
    match msg with
    | UserProfileMsg msg' ->
        { state with UserProfileForm = LightForm.States.update Map.empty msg' state.UserProfileForm }
        , Cmd.none