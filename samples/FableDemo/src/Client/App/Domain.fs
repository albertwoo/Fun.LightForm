[<AutoOpen>]
module rec Client.App.Domain

open System
open Fun.LightForm

type State =
  { UserProfileForm: LightForm }

type Msg =
  | UserProfileMsg of LightFormMsg


type Page =
  | Home of string
  | About
  | Blog of int option
  | Loading
  | NotFound of string


type UserProfile =
  { UserName: string
    Password: string
    Birthday: DateTime
    Roles: int list
    DefaultRole: int
    Country: int option
    Address: Address }
  static member defaultValue =
    { UserName = "email"
      Password = "password"
      Birthday = DateTime.Now
      Roles = []
      DefaultRole = -1
      Address = Address.defaultValue
      Country = None }
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
