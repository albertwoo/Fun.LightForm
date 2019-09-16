[<AutoOpen>]
module rec Client.App.Domain

open System
open Fun

type State =
  { UserProfileForm: LightForm.Domain.Model }

type Msg =
  | UserProfileMsg of LightForm.Domain.Msg


type Page =
  | Home of string
  | About
  | Blog of int option
  | Loading
  | NotFound of string


type UserProfile =
  { UserName: string
    Password: string
    Roles: string list
    Address: Address }
  static member defaultValue =
    { UserName = ""
      Password = ""
      Roles = []
      Address = Address.defaultValue }
and Address =
  { Country: string
    City: string
    Street: string option
    ZipCode: int }
  static member defaultValue =
    { Country = ""
      City = ""
      Street = None
      ZipCode = 12 }
